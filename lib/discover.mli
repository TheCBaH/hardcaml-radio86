(** Finding the probes of a manifest in a trace from the declarations of its header, listed by ovcd
    ([Vcd_signals], what [vcd_dump --list-signals --format jsonl] prints). It works on a VCD and on an FST
    alike, and reads no value: a manifest is data, so this only suggests, never binds. *)

val signals : Hwfrp.Trace.t -> Vcd_signals.t list
(** Every declaration of the trace, in header order. *)

type probe = { name : string; path : string list; width : int }
(** What a manifest needs of the trace: the clock, its enable, and each signal and its [valid]. *)

val probes : Manifest.t -> probe list

type found =
  | Present  (** The path is declared, with this width. *)
  | Moved of Vcd_signals.t list
      (** Not there, but these declarations have the same leaf name (ignoring case, a [pin_] prefix and trailing underscores) and
          the width. *)
  | Missing

val find : Vcd_signals.t list -> probe -> found
val check : Manifest.t -> Vcd_signals.t list -> (probe * found) list

val hint : Vcd_signals.t list -> probe -> string option
(** For a probe that is not [Present]: the suggestion line, or [None] when there is none. *)

val repath : Manifest.t -> Vcd_signals.t list -> (Manifest.t, string list) result
(** The manifest with every [Moved] probe that has a single candidate pointed at it. An error lists the
    probes that stay [Missing] or have several candidates. *)
