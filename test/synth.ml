(* A waveform built from frames, so that the models run through hwfrp without a simulator: a
   pin_clk with two rising edges per T-state, f2 high at the second, and the pins changing there. *)
open I8080_trace
open Hwfrp
open Vcd_types

let ids = Hashtbl.create 32

let id_of name =
  match Hashtbl.find_opt ids name with
  | Some i -> i
  | None ->
      let i = Printf.sprintf "s%d" (Hashtbl.length ids) in
      Hashtbl.replace ids name i;
      i

let value ~width = function
  | None ->
      if width = 1 then Value.Scalar Value.X else Value.Scalars (List.init width (fun _ -> Value.X))
  | Some v ->
      if width = 1 then Value.Scalar (if v <> 0 then Value.B1 else Value.B0) else Value.Int v

let bit_int = function None -> None | Some b -> Some (if b then 1 else 0)

let reg_value (f : Frame.t) name =
  match f.regs with
  | None -> None
  | Some r -> (
      match name with
      | "acc" -> r.acc
      | "bc" -> r.bc
      | "de" -> r.de
      | "hl" -> r.hl
      | "sp" -> r.sp
      | "pc" -> r.pc
      | "wz" -> r.wz
      | "z" -> r.z
      | "s" -> r.s
      | "p" -> r.p
      | "c" -> r.c
      | "ac" -> r.ac
      | n -> failwith ("register " ^ n))

let pin_value (f : Frame.t) = function
  | "a" -> f.a
  | "do" -> f.dout
  | "di" -> f.din
  | "sync" -> bit_int f.sync
  | "dbin" -> bit_int f.dbin
  | "wr_n" -> bit_int f.wr_n
  | "wait" -> bit_int f.wait
  | "hlda" -> bit_int f.hlda
  | "inte" -> bit_int f.inte
  | "ready" -> bit_int f.ready
  | "hold" -> bit_int f.hold
  | "int" -> bit_int f.int_
  | "reset" -> bit_int f.reset
  | n -> failwith ("pin " ^ n)

(* All signals live in the scope named by the first path component. *)
let trace ?(drop = []) ?(regs = false) (m : Manifest.t) frames =
  let open In_memory in
  Hashtbl.reset ids;
  let sigs =
    List.filter
      (fun (s : Manifest.signal) -> (s.kind = Manifest.Pin || regs) && not (List.mem s.name drop))
      m.signals
  in
  let leaf (s : Manifest.signal) = List.nth s.path (List.length s.path - 1) in
  let vars =
    List.map (fun s -> var ~id:(id_of s.Manifest.name) ~width:s.Manifest.width (leaf s)) sigs
    @ [ var ~id:"clk" ~width:1 "clk"; var ~id:"f2" ~width:1 "f2" ]
  in
  let header = header [ scope "i8080_tb" vars ] in
  let bit b = Value.Scalar (if b then Value.B1 else Value.B0) in
  let evs = ref [] in
  let add e = evs := e :: !evs in
  add (at 0L);
  add (change ~id:"clk" (bit false));
  add (change ~id:"f2" (bit false));
  List.iteri
    (fun k (f : Frame.t) ->
      let t = 20 * k in
      let at' d = add (at (Int64.of_int (t + d))) in
      at' 1;
      add (change ~id:"f2" (bit false));
      at' 2;
      add (change ~id:"clk" (bit true));
      at' 5;
      add (change ~id:"clk" (bit false));
      at' 7;
      add (change ~id:"f2" (bit true));
      at' 9;
      add (change ~id:"clk" (bit true));
      List.iter
        (fun s ->
          add
            (change ~id:(id_of s.Manifest.name)
               (value ~width:s.Manifest.width
                  (if s.Manifest.kind = Manifest.Pin then pin_value f s.Manifest.name
                   else reg_value f s.Manifest.name))))
        sigs;
      at' 12;
      add (change ~id:"clk" (bit false)))
    frames;
  In_memory.trace header (List.to_seq (List.rev !evs))
