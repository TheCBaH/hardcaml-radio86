(** The manifest bound to a waveform with hwfrp, sampled once per T-state, and fed to {!Pipeline}. A path of
    the manifest that the trace does not have, or a width that differs, is an error naming every such
    probe: nothing is defaulted. *)

type result = {
  events : Events.t list;
  frames : Frame.t list;  (** Empty unless requested. *)
  tstates : int;
  diagnostics : Hwfrp.Diag.t list;
}

val bind_check : Manifest.t -> Hwfrp.Trace.t -> (unit, string list) Stdlib.result
(** Whether every probe of the manifest resolves. *)

val run : ?frames:bool -> Manifest.t -> Hwfrp.Trace.t -> (result, string list) Stdlib.result
