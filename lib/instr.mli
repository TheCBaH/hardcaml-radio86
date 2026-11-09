(** The instruction model: instruction spans from the bus, decoded with Rivet's i8080 decoder, and the
    register snapshots around them. An instruction starts at its opcode fetch and ends where the next one
    starts (or at the halt acknowledge); its [after] registers are read at the point the manifest names, so
    never in the middle of the next fetch's work. *)

type t = {
  idx : int;
  epoch : int;
  pc : int option;  (** The address of the opcode fetch. *)
  bytes : int list;  (** The opcode and operand bytes as fetched. *)
  expected_len : int option;  (** From Rivet's descriptor table. *)
  text : string option;  (** Rivet's canonical text; [None] when the bytes are incomplete. *)
  first : int;
  last : int;
      (** The T-state before the next instruction's [T1] (internal T-states included), or the first [WAIT]
          sample for [HLT]; the last transfer's T-state when no next instruction began. *)
  tstates : int;
  accesses : Bus.cycle list;  (** Every accepted transfer, in bus order. *)
  before : Regs.t option;
  after : Regs.t option;
  complete : bool;
  why : string;  (** Why it is not complete. *)
}

type out =
  | Instruction of t
  | Halt_evidence of { first : int; samples : (int * int option * int option) list }
      (** The halt cycle's samples: T-state, [A] pin and the internal [PC]; evidence only. *)
  | Note of { at : int; msg : string }

type state

val init : Manifest.snapshot -> state
val step : state -> Frame.t -> Bus.out list -> state * out list

val finish : state -> Bus.out list -> out list
(** End of the capture, with what {!Bus.finish} reports. *)

val describe : int list -> origin:int -> int option * string option
(** The length Rivet's table gives for the opcode and the canonical text of a complete byte string. *)
