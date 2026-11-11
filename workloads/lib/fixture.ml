(* A workload as data: an image of code, an image of RAM (inputs, zeroed result and scratch areas,
   guards), the regions a run may write, what must be in RAM afterwards and a T-state budget.
   Authored in this repository (MIT).

   The map is the same for every workload: code at 0, so that reset enters it; data at [data_origin];
   the stack below [stack_top], with a guard under it. Code, inputs, scratch, results and stack are
   disjoint, and the data image carries a guard on each side of its writable areas. *)

module Asm = I8080_asm.Program

let data_origin = 0x4000
let stack_top = 0xf000
let stack_size = 0x400
let guard_byte = 0xa5
let guard_size = 8

type region = { name : string; lo : int; len : int }

type t = {
  name : string;
  sources : string list;  (** the ported routines and their origin, for the report *)
  code : Asm.image;  (** loaded at its origin; the entry is that origin *)
  data : Asm.image;  (** inputs, zeroed outputs, scratch and guards, loaded at its origin *)
  extra : (int * string) list;  (** more initial RAM: the stack guard *)
  writable : region list;  (** every region a run may write, the stack included *)
  expect : (int * string) list;  (** address and bytes that RAM must hold at HLT *)
  final_sp : int;  (** the stack pointer at HLT, stored by the driver in [sp_save] *)
  budget : int;  (** T-states, from measured runs with a margin *)
}

let symbol_exn (img : Asm.image) name =
  match Asm.symbol img name with Some a -> a | None -> invalid_arg ("Fixture: no symbol " ^ name)

let word_bytes w = String.init 2 (fun i -> Char.chr ((w lsr (8 * i)) land 0xff))
let words ws = String.concat "" (List.map word_bytes ws)
let guard = String.make guard_size (Char.chr guard_byte)

(* The stack region and its guard. *)
let stack_region = { name = "stack"; lo = stack_top - stack_size; len = stack_size }
let stack_guard = (stack_region.lo - guard_size, guard)

(* The initial RAM of a fixture, in load order. *)
let image t =
  (t.code.Asm.origin, t.code.Asm.bytes) :: (t.data.Asm.origin, t.data.Asm.bytes) :: t.extra

(* What a finished run reports, whatever executed it. *)
type outcome = {
  halted : bool;  (** stopped at HLT *)
  trapped : bool;
  tstates : int;  (** T-states from the first fetch to the stop *)
  writes : (int * int) list;  (** address and byte of every memory write, in order *)
  port_traffic : bool;  (** any IN or OUT *)
  memory : Bytes.t;  (** the final 64 KiB *)
}

let within regions a = List.exists (fun r -> a >= r.lo && a < r.lo + r.len) regions

(* Every complaint about [o], empty when the run is as the fixture requires. *)
let check t (o : outcome) =
  let errs = ref [] in
  let fail fmt = Printf.ksprintf (fun s -> errs := s :: !errs) fmt in
  if o.trapped then fail "trapped";
  if not o.halted then fail "did not reach HLT";
  if o.tstates > t.budget then fail "%d T-states exceed the budget of %d" o.tstates t.budget;
  if o.port_traffic then fail "port traffic";
  (match List.find_opt (fun (a, _) -> not (within t.writable a)) o.writes with
  | Some (a, v) -> fail "write of %02x to %04x outside the writable regions" v a
  | None -> ());
  List.iter
    (fun (a, bytes) ->
      let got = Bytes.sub_string o.memory a (String.length bytes) in
      if got <> bytes then
        fail "RAM at %04x: expected %s, got %s" a
          (String.concat " "
             (List.init (String.length bytes) (fun i -> Printf.sprintf "%02x" (Char.code bytes.[i]))))
          (String.concat " "
             (List.init (String.length got) (fun i -> Printf.sprintf "%02x" (Char.code got.[i])))))
    t.expect;
  (* the guards: the first and last bytes of the data image, and the one under the stack *)
  let guards =
    [
      ("data low", t.data.Asm.origin);
      ("data high", t.data.Asm.origin + String.length t.data.Asm.bytes - guard_size);
      ("stack", fst stack_guard);
    ]
  in
  List.iter
    (fun (what, a) ->
      if Bytes.sub_string o.memory a guard_size <> guard then
        fail "the %s guard at %04x was overwritten" what a)
    guards;
  List.rev !errs

(* The lowest stack address written: how deep the stack went. *)
let stack_depth (o : outcome) =
  List.fold_left
    (fun low (a, _) -> if a >= stack_region.lo && a < stack_top then min low a else low)
    stack_top o.writes
  |> fun low -> stack_top - low
