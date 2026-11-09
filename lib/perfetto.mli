(** A Chrome trace (JSON trace event format; ui.perfetto.dev and chrome://tracing load it) with lanes for
    the instructions, the bus transfers and the control events, and a counter per register. Time is the
    T-state number, written as microseconds. *)

val render : Manifest.t -> Events.t list -> string
