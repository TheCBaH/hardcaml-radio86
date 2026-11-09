(** The decoded events of an observation and their three renderings. The JSON lines and the text are
    deterministic functions of the events, so a replay can be compared byte for byte. *)

type t =
  | Bus of Bus.out
  | Instr of Instr.out
  | End of { tstates : int; first_t1 : int option }
      (** The last event: how many T-states were sampled and the number of the first [T1]. *)

val header : Manifest.t -> string
(** The first line of a JSONL stream: format and the manifest's identity. *)

val to_json : t -> string option
(** One JSON object on one line; [None] for the events that are only inputs of other models
    ({!Bus.Start}). *)

val to_text : t -> string option
val is_instruction : t -> Instr.t option
val transfers : t list -> Bus.cycle list
val instructions : t list -> Instr.t list
