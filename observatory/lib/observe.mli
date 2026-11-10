(** The manifest bound to a waveform with hwfrp, sampled once per T-state, and fed to {!Pipeline}. A path of
    the manifest that the trace does not have, or a width that differs, is an error naming every such
    probe: nothing is defaulted. *)

type result = {
  events : Events.t list;
  frames : Frame.t list;  (** Empty unless requested. *)
  tstates : int;
  enabled : int;
      (** Samples where the clock's enable was high: one per T-state, so equal to [tstates]. *)
  diagnostics : Hwfrp.Diag.t list;
      (** What the bindings reported, then one per kind of signal behaviour (never driven, constant, unknown at the end). *)
}

val bind_check : Manifest.t -> Hwfrp.Trace.t -> (unit, string list) Stdlib.result
(** Whether every probe of the manifest resolves, found in one pass ([Hwfrp.Bind.collect]). *)

val run : ?frames:bool -> Manifest.t -> Hwfrp.Trace.t -> (result, string list) Stdlib.result
