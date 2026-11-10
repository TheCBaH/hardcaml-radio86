(** An observation on disk: the manifest as run (naming the trace by SHA-256), the trace itself, the events
    in three renderings, and what is needed to replay them. *)

val jsonl : Manifest.t -> Events.t list -> string
val text : Events.t list -> string

val write : dir:string -> Manifest.t -> trace:string -> Events.t list -> (unit, string) result
(** Writes [dir/manifest.json], [dir/trace.fst] (a copy of [trace]), [dir/events.jsonl], [dir/events.txt]
    and [dir/perfetto.json]. *)

val replay : dir:string -> (string list, string) result
(** Reads [dir/manifest.json] and the trace it names, verifies the trace's SHA-256, observes it again and
    returns the names of the files whose bytes differ from the recorded ones (empty: a byte-exact replay). *)
