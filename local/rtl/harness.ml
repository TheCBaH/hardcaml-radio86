(* Cyclesim harness around [Core]: 64 KiB RAM, a K4 stimulus, the K4 pin trace and, optionally, the
   waveform written by Vcd.wrap for hwfrp.

   The inputs of T-state k are chosen from the pins of T-state k, which are the outputs after the
   previous clock edge (the core is Moore, so they do not depend on the inputs): the memory answers
   the address it sees, and READY follows the schedule. *)
open Hardcaml
module Sim = Cyclesim.With_interface (Core.I) (Core.O)

type sample = {
  n : int;  (** T-state number, counted from the first cycle of the run *)
  a : int;
  dout : int option;  (** [None] when the core does not drive the data bus *)
  din : int option;  (** [None] when the harness does not drive it *)
  sync : bool;
  dbin : bool;
  wr_n : bool;
  wait : bool;
  hlda : bool;
  inte : bool;
  ready : bool;
  hold : bool;
  int_ : bool;
  reset : bool;
  tstate : int;
  cycle : int;
  accept : bool;
  acc : int;
  pc : int;
  trap : bool;
  trap_cause : int;
}

type write = { at : int; addr : int; data : int }

type result = {
  samples : sample list;
  writes : write list;  (** one entry per WR pulse *)
  counted : int option;  (** K2: first SYNC through the first TWH, [None] if the run never halted *)
  pre_sync : int;  (** samples before the first SYNC *)
  trapped : (int * int) option;  (** sample and cause of the first trap *)
  memory : Bytes.t;
}

let bit r = Bits.to_int !r = 1
let int r = Bits.to_int !r

let pins_of (o : Bits.t ref Core.O.t) =
  (int o.a, int o.dout, bit o.dout_en, bit o.sync, bit o.dbin, bit o.wr_n, bit o.wait_)

(* The pins of the cycle that has not started: reset state, nothing driven, WR inactive. *)
let idle_pins = (0, 0, false, false, false, true, false)

let load_program memory ~origin bytes =
  if origin < 0 || origin + String.length bytes > Bytes.length memory then
    invalid_arg "Harness.load_program: image does not fit in 64 KiB";
  Bytes.blit_string bytes 0 memory origin (String.length bytes)

let new_memory ?(fill = 0) () = Bytes.make 0x10000 (Char.chr fill)

(* [tick_every] > 1 runs the implementation clock faster than the T-state: the CPU advances only in the
   cycle where [tick] is high, the inputs are held in the others. [halt_samples] is how many halted (or
   trapped) samples to record after the first. *)
let run ?(tick_every = 1) ?(halt_samples = 2) ?(max_tstates = 100_000) ?vcd ?(stim = Stimulus.empty)
    ~memory () =
  if tick_every < 1 then invalid_arg "Harness.run: tick_every";
  let sim = Sim.create Core.create in
  let sim = match vcd with Some oc -> Vcd.wrap oc sim | None -> sim in
  let i = Cyclesim.inputs sim and o = Cyclesim.outputs ~clock_edge:Before sim in
  let after = Cyclesim.outputs ~clock_edge:After sim in
  Cyclesim.reset sim;
  let set r b = r := if b then Bits.vdd else Bits.gnd in
  let samples = ref [] and writes = ref [] in
  let pending = ref None in
  let t1s = Hashtbl.create 16 in
  let cycles = ref (-1) in
  let first_sync = ref None and halted_at = ref None and trapped = ref None in
  let stop = ref None in
  let pins = ref idle_pins in
  let din = ref 0 and prev_dbin = ref false in
  let n = ref 0 in
  let commit_write () =
    match !pending with
    | Some (at, addr, data) ->
        Bytes.set memory addr (Char.chr data);
        writes := { at; addr; data } :: !writes;
        pending := None
    | None -> ()
  in
  while !stop = None && !n < max_tstates do
    let k = !n in
    let a, dout, dout_en, sync, dbin, wr_n, wait = !pins in
    if sync then begin
      incr cycles;
      Hashtbl.replace t1s !cycles k
    end;
    let t1 c = Hashtbl.find_opt t1s c in
    let sig_ s = Stimulus.value stim ~t1 ~n:k s in
    let ready = sig_ Ready and hold = sig_ Hold and int_ = sig_ Int and reset = sig_ Reset in
    if reset then begin
      cycles := -1;
      Hashtbl.reset t1s
    end;
    (* the memory answers while DBIN is high; the byte stays for the sample that ends the read *)
    let drives = dbin || !prev_dbin in
    if dbin then din := Char.code (Bytes.get memory a);
    (match (wr_n, dout_en) with
    | false, true -> pending := Some (k, a, dout)
    | true, _ -> commit_write ()
    | false, false -> failwith "Harness: WR low while the data bus is not driven");
    set i.ready ready;
    set i.hold hold;
    set i.int_req int_;
    set i.rst_in reset;
    i.di := Bits.of_int ~width:8 !din;
    for _ = 2 to tick_every do
      set i.tick false;
      Cyclesim.cycle sim
    done;
    set i.tick true;
    Cyclesim.cycle sim;
    (* the pins the sample sees are those of the cycle just run: they must be the ones assumed above *)
    let seen = pins_of o in
    if seen <> !pins && k > 0 then
      failwith (Printf.sprintf "Harness: the pins of T-state %d depend on the inputs" k);
    let s =
      {
        n = k;
        a;
        dout = (if dout_en then Some dout else None);
        din = (if drives then Some !din else None);
        sync;
        dbin;
        wr_n;
        wait;
        hlda = bit o.hlda;
        inte = bit o.inte;
        ready;
        hold;
        int_;
        reset;
        tstate = int o.tstate;
        cycle = int o.cycle;
        accept = bit o.accept;
        acc = int o.acc;
        pc = int o.pc;
        trap = bit o.trap;
        trap_cause = int o.trap_cause;
      }
    in
    samples := s :: !samples;
    if sync && !first_sync = None then first_sync := Some k;
    if s.tstate = Core.twh && !halted_at = None then halted_at := Some k;
    if s.trap && !trapped = None then trapped := Some (k, s.trap_cause);
    prev_dbin := dbin;
    pins := pins_of after;
    incr n;
    let ends =
      match (!halted_at, !trapped) with Some h, _ -> Some h | _, Some (t, _) -> Some t | _ -> None
    in
    match ends with Some e when k >= e + halt_samples -> stop := Some () | _ -> ()
  done;
  commit_write ();
  let samples = List.rev !samples in
  let counted =
    match (!first_sync, !halted_at) with Some s, Some h -> Some (h - s + 1) | _ -> None
  in
  {
    samples;
    writes = List.rev !writes;
    counted;
    pre_sync = Option.value !first_sync ~default:(List.length samples);
    trapped = !trapped;
    memory;
  }

(* The K4 pin trace (i8080-pin-trace v1): n = 0 is the first SYNC sample. *)
let pin_trace ?(source = "hardcaml-cyclesim") (r : result) =
  let b = Buffer.create 1024 in
  let h w = function None -> String.make w 'z' | Some v -> Printf.sprintf "%0*x" w v in
  let bit x = if x then '1' else '0' in
  Buffer.add_string b "# i8080-pin-trace v1\n";
  Buffer.add_string b
    (Printf.sprintf "# source=%s profile=i8080a/1 pre_sync=%d\n" source r.pre_sync);
  Buffer.add_string b "# n a do di sync dbin wr_n wait hlda inte ready hold int reset\n";
  List.iter
    (fun s ->
      if s.n >= r.pre_sync then
        Buffer.add_string b
          (Printf.sprintf "%d %04x %s %s %c %c %c %c %c %c %c %c %c %c\n" (s.n - r.pre_sync) s.a
             (h 2 s.dout) (h 2 s.din) (bit s.sync) (bit s.dbin) (bit s.wr_n) (bit s.wait)
             (bit s.hlda) (bit s.inte) (bit s.ready) (bit s.hold) (bit s.int_) (bit s.reset)))
    r.samples;
  Buffer.contents b
