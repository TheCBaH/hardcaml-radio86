(** The observation manifest: which waveform signals are the pins and the registers of an 8080, how the
    clock is sampled and what one T-state is. It is data, never probed: a path that the trace lacks is a
    hard error (see {!Observe}). Format ["i8080-observation-manifest/1"]. *)

type rule = After_rising | After_falling | Before_rising

type clock = {
  path : string list;
  rule : rule;
  enable : string list option;
      (** A 1-bit signal; the sample is a T-state only where it is high (the second [pin_clk] edge of a vm80a
          T-state, the sample strobe of a CPU with a faster implementation clock). *)
}

type kind = Pin | Register

type signal = {
  name : string;  (** One of {!pin_names} or {!register_names}. *)
  path : string list;
  width : int;
  kind : kind;
}

type snapshot =
  | Next_fetch  (** Registers are read in the [T1] sample of the next instruction's first cycle. *)
  | Last_tstate  (** Registers are read in the last T-state of the instruction's last cycle. *)

type trace_ref = { file : string; format : string; sha256 : string; bytes : int }

type t = {
  id : string;
  profile : string;  (** ["i8080a/1"]: the CPU profile of the contracts (K1). *)
  description : string;
  clock : clock;
  signals : signal list;
  snapshot : snapshot;
  trace : trace_ref option;  (** Set in an exported manifest: the trace it was made from. *)
}

val format : string
val profile : string

val pin_names : (string * int) list
(** The 13 pins of the profile, with their widths: every one is required. *)

val register_names : (string * int * bool) list
(** The internal registers a manifest may declare, with their widths and whether a snapshot needs them.
    Declaring none is allowed (registers are then reported as unobserved); declaring some but not all of the
    required ones is an error. *)

val find : t -> string -> signal option
val has_registers : t -> bool
val validate : t -> (unit, string list) result
val of_string : ?file:string -> string -> (t, string) result
val load : string -> (t, string) result
val to_string : t -> string
val rule_name : rule -> string
