(** One T-state of an 8080: the pins as the contracts' pin trace (K4) shows them, and, when the manifest
    declares them, the internal registers. A value that is not known (X or Z in the trace, or masked in a
    hand-written trace) is [None]; it is never replaced by zero. *)

type regs = {
  acc : int option;
  bc : int option;
  de : int option;
  hl : int option;
  sp : int option;
  pc : int option;
  wz : int option;
  z : int option;
  s : int option;
  p : int option;
  c : int option;
  ac : int option;
}

type t = {
  n : int;  (** The T-state number: 0 at the first sample of the capture. *)
  time : int64;  (** Trace timestamp, in the units of the trace; 0 for a trace without time. *)
  a : int option;
  dout : int option;
  din : int option;
  sync : bool option;
  dbin : bool option;
  wr_n : bool option;
  wait : bool option;
  hlda : bool option;
  inte : bool option;
  ready : bool option;
  hold : bool option;
  int_ : bool option;
  reset : bool option;
  regs : regs option;  (** [None]: the manifest declares no registers. *)
}

val no_regs : regs
(** Every register unknown. *)

val pp : Format.formatter -> t -> unit
