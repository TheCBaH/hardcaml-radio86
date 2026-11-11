(* Cyclesim harness around [Core]: 64 KiB RAM, I/O ports, a K4 stimulus, the K4 pin trace and,
   optionally, the waveform written by Vcd.wrap for hwfrp. It does what the vm80a harness does: a cycle
   whose status word at SYNC is IN, OUT or an interrupt acknowledge never touches memory (the stimulus
   supplies the bytes of INTA cycles), a boot preamble may run from 0 until the first fetch at its end
   address, and a run stops at the halt cycle or after an OUT to a stop port.

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
}

type write = { at : int; addr : int; data : int }
type stop = Halt | Port of int | Budget

type boot = {
  code : string;  (** loaded at 0 over the memory, which is restored when the run reaches [pc] *)
  pc : int;  (** the address of the first fetch that counts *)
}

type result = {
  samples : sample list;
  io_out : (int * int) list;  (** (port, byte) of each OUT, in order *)
  stop : stop;
  writes : write list;  (** one entry per WR pulse *)
  counted : int option;  (** K2: first SYNC through the first TWH, [None] if the run never halted *)
  pre_sync : int;  (** samples before the first SYNC *)
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

(* K5: the CP/M shim, the same 34 bytes in every runner. *)
let shim_handler =
  "\x79\xfe\x02\xca\x0e\xfe\xfe\x09\xca\x12\xfe\xd3\xfe\xc9\x7b\xd3\x01\xc9\x1a\xfe\x24\xc8\xd3\x01\x13\xc3\x12\xfe"

let load_shim memory =
  load_program memory ~origin:0 "\xd3\xff\x76";
  load_program memory ~origin:5 "\xc3\x00\xfe";
  load_program memory ~origin:0xfe00 shim_handler

(* The preamble of the release's .COM runs: SP, BC, DE, HL and A set, then a jump to the program. *)
let com_boot =
  {
    code = "\x31\x00\xfe\x01\x00\x00\x11\x00\x00\x21\x00\x00\x3e\x01\xb7\x3e\x00\xc3\x00\x01";
    pc = 0x100;
  }

(* [tick_every] > 1 runs the implementation clock faster than the T-state: the CPU advances only in the
   cycle where [tick] is high, the inputs are held in the others. [halt_samples] is how many halted (or
   stopped) samples to record after the first; with [halt_stop] false the halt cycle does not end the
   run (an interrupt may). [probe] sees every clock cycle: the inputs
   applied and the outputs before the edge. *)
let run ?(tick_every = 1) ?(halt_samples = 2) ?(halt_stop = true) ?(max_tstates = 100_000) ?vcd
    ?probe ?(stim = Stimulus.empty) ?(in_default = 0xff) ?(stop_ports = []) ?boot ~memory () =
  if tick_every < 1 then invalid_arg "Harness.run: tick_every";
  let sim = Sim.create Core.create in
  let sim = match vcd with Some oc -> Vcd.wrap oc sim | None -> sim in
  let i = Cyclesim.inputs sim and o = Cyclesim.outputs ~clock_edge:Before sim in
  let after = Cyclesim.outputs ~clock_edge:After sim in
  Cyclesim.reset sim;
  let set r b = r := if b then Bits.vdd else Bits.gnd in
  let saved_boot =
    Option.map
      (fun b ->
        let saved = Bytes.sub memory 0 (String.length b.code) in
        Bytes.blit_string b.code 0 memory 0 (String.length b.code);
        (b, saved))
      boot
  in
  let booting = ref (saved_boot <> None) in
  let samples = ref [] and writes = ref [] and io_out = ref [] in
  let pending = ref None in
  let t1s = Hashtbl.create 16 in
  let cycles = ref (-1) in
  let first_sync = ref None and halted_at = ref None in
  let stop_at = ref None and stopped = ref None in
  let stop = ref None in
  let pins = ref idle_pins in
  let din = ref 0 and prev_dbin = ref false in
  let kind = ref `Mem_read in
  let in_used = Hashtbl.create 4 and inta_used = ref 0 in
  let n = ref 0 in
  let commit_write () =
    match !pending with
    | Some (at, addr, data, kind) ->
        (match kind with
        | `Out ->
            io_out := (addr land 0xff, data) :: !io_out;
            if List.mem (addr land 0xff) stop_ports && !stop_at = None then stop_at := Some (-1)
        | _ ->
            Bytes.set memory addr (Char.chr data);
            writes := { at; addr; data } :: !writes);
        pending := None
    | None -> ()
  in
  while !stop = None && !n < max_tstates do
    let k = !n in
    let a, dout, dout_en, sync, dbin, wr_n, wait = !pins in
    if sync then begin
      incr cycles;
      Hashtbl.replace t1s !cycles k;
      (kind :=
         match dout with
         | 0x42 -> `In
         | 0x10 -> `Out
         | s when s land 1 = 1 -> `Inta
         | s -> if s land 2 = 0 then `Mem_write else `Mem_read);
      (match !booting with
      | true when dout = 0xa2 && match saved_boot with Some (b, _) -> a = b.pc | None -> false ->
          let _, saved = Option.get saved_boot in
          Bytes.blit saved 0 memory 0 (Bytes.length saved);
          booting := false;
          cycles := 0;
          Hashtbl.reset t1s;
          Hashtbl.replace t1s 0 k;
          first_sync := Some k
      | _ -> ());
      if !first_sync = None && not !booting then first_sync := Some k
    end;
    let t1 c = Hashtbl.find_opt t1s c in
    let sig_ s = Stimulus.value stim ~t1 ~n:k s in
    let ready = sig_ Ready and hold = sig_ Hold and int_ = sig_ Int and reset = sig_ Reset in
    if reset then begin
      cycles := -1;
      Hashtbl.reset t1s
    end;
    (* the memory (or the port) answers while DBIN is high; the byte stays for the sample that ends the read *)
    let drives = dbin || !prev_dbin in
    if dbin then
      begin match !kind with
      | `In ->
          if not !prev_dbin then begin
            let port = a land 0xff in
            let used = Option.value (Hashtbl.find_opt in_used port) ~default:0 in
            Hashtbl.replace in_used port (used + 1);
            din := Option.value (Stimulus.io_byte stim ~port ~n:used) ~default:in_default
          end
      | `Inta ->
          if not !prev_dbin then begin
            din := Option.value (List.nth_opt stim.inta !inta_used) ~default:in_default;
            incr inta_used
          end
      | _ -> din := Char.code (Bytes.get memory a)
      end;
    (match (wr_n, dout_en) with
    | false, true -> pending := Some (k, a, dout, !kind)
    | true, _ -> commit_write ()
    | false, false -> failwith "Harness: WR low while the data bus is not driven");
    set i.ready ready;
    set i.hold hold;
    set i.int_req int_;
    set i.rst_in reset;
    i.di := Bits.of_int ~width:8 !din;
    (* the pins and the state of a T-state are read in its first clock cycle: RESET clears the registers
       without waiting for [tick], so the later cycles of a slower T-state may already show the cleared
       state. The pins must be the ones assumed above. *)
    let first = ref None in
    for j = 1 to tick_every do
      set i.tick (j = tick_every);
      Cyclesim.cycle sim;
      Option.iter (fun f -> f i o) probe;
      if j = 1 then begin
        if pins_of o <> !pins && k > 0 then
          failwith (Printf.sprintf "Harness: the pins of T-state %d depend on the inputs" k);
        first :=
          Some
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
            }
      end
    done;
    let s = Option.get !first in
    samples := s :: !samples;
    if halt_stop && s.tstate = Core.twh && !halted_at = None then halted_at := Some k;
    (* an OUT to a stop port ends the run at the first sample after its WR pulse *)
    (match !stop_at with
    | Some -1 when wr_n -> stop_at := Some k
    | _ -> ());
    prev_dbin := dbin;
    pins := pins_of after;
    incr n;
    (* like the vm80a harness: two samples after the halt cycle's first, one after the stop sample *)
    let ends =
      match (!halted_at, !stop_at) with
      | Some h, _ -> Some (h + halt_samples)
      | _, Some k' when k' >= 0 -> Some (k' + halt_samples - 1)
      | _ -> None
    in
    match ends with Some e when k >= e -> stop := Some () | _ -> ()
  done;
  commit_write ();
  ignore stopped;
  let samples = List.rev !samples in
  let pre_sync = Option.value !first_sync ~default:(List.length samples) in
  let stop_reason =
    match (!halted_at, !stop_at) with
    | Some _, _ -> Halt
    | _, Some _ -> ( match !io_out with (p, _) :: _ -> Port p | [] -> Budget)
    | _ -> Budget
  in
  let counted =
    match (!halted_at, !stop_at) with
    | Some h, _ -> Some (h - pre_sync + 1)
    | _, Some s when s >= 0 -> Some (s - pre_sync)
    | _ -> None
  in
  {
    samples;
    io_out = List.rev !io_out;
    stop = stop_reason;
    writes = List.rev !writes;
    counted;
    pre_sync;
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
