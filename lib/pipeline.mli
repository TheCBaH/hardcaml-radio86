(** The models chained: bus, then instructions and registers. Pure: the same frames give the same events,
    whether they come from a waveform through hwfrp ({!Observe}), from a hand-written trace or from a
    recorded sample file. *)

type t

val init : Manifest.snapshot -> t
val step : t -> Frame.t -> t * Events.t list
val finish : t -> Events.t list
val run : Manifest.snapshot -> Frame.t Seq.t -> Events.t list
