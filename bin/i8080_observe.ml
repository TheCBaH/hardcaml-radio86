open Cmdliner
open I8080_trace

let read_file f = In_channel.with_open_bin f In_channel.input_all
let write_file f s = Out_channel.with_open_bin f (fun oc -> Out_channel.output_string oc s)

let die fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline s;
      exit 2)
    fmt

let load_manifest file =
  let m = match Manifest.load file with Ok m -> m | Error e -> die "%s: %s" file e in
  (match Manifest.validate m with
  | Ok () -> ()
  | Error l -> die "%s: %s" file (String.concat "\n  " l));
  m

let open_trace file =
  try Hwfrp_native.open_file file with e -> die "%s: %s" file (Printexc.to_string e)

let observe_trace m trace =
  match Observe.run m (open_trace trace) with
  | Error l -> die "%s" (String.concat "\n" l)
  | Ok r -> r

let manifest_arg =
  Arg.(
    required & opt (some file) None & info [ "manifest" ] ~docv:"FILE" ~doc:"Observation manifest.")

let trace_arg =
  Arg.(required & opt (some file) None & info [ "trace" ] ~docv:"FST" ~doc:"Waveform (FST or VCD).")

let out_opt names doc = Arg.(value & opt (some string) None & info names ~docv:"FILE" ~doc)

let observe manifest trace jsonl text perfetto export show =
  let m = load_manifest manifest in
  let r = observe_trace m trace in
  Option.iter (fun f -> write_file f (Export.jsonl m r.events)) jsonl;
  Option.iter (fun f -> write_file f (Export.text r.events)) text;
  Option.iter (fun f -> write_file f (Perfetto.render m r.events)) perfetto;
  Option.iter
    (fun d ->
      match Export.write ~dir:d m ~trace r.events with Ok () -> () | Error e -> die "%s" e)
    export;
  if show then print_string (Export.text r.events);
  Printf.eprintf "%d T-states, %d events\n" r.tstates (List.length r.events)

let observe_cmd =
  Cmd.v
    (Cmd.info "observe" ~doc:"Decode a waveform into bus, instruction and register events.")
    Term.(
      const observe $ manifest_arg $ trace_arg
      $ out_opt [ "jsonl" ] "Write the events as JSON lines."
      $ out_opt [ "text" ] "Write the events as text."
      $ out_opt [ "perfetto" ] "Write a Chrome/Perfetto trace."
      $ out_opt [ "export" ] "Write the replayable export directory."
      $ Arg.(value & flag & info [ "print" ] ~doc:"Print the text events."))

let check manifest trace =
  let m = load_manifest manifest in
  match Observe.bind_check m (open_trace trace) with
  | Ok () -> Printf.printf "%s: every probe of %s is in the trace\n" trace m.id
  | Error l ->
      List.iter prerr_endline l;
      exit 1

let check_cmd =
  Cmd.v
    (Cmd.info "check" ~doc:"Check that every probe of the manifest resolves in the trace.")
    Term.(const check $ manifest_arg $ trace_arg)

(* A hand-written K4 pin trace through the same models, optionally compared with a capture. *)
let k4 manifest hand against =
  let m = load_manifest manifest in
  let frames = match K4.parse (read_file hand) with Ok f -> f | Error e -> die "%s: %s" hand e in
  let evs = Pipeline.run m.snapshot (List.to_seq frames) in
  match against with
  | None -> print_string (Export.text evs)
  | Some trace ->
      let r = observe_trace m trace in
      let offset = Option.value (K4.first_t1 r.events) ~default:0 in
      let d =
        K4.compare_transfers ~expected:(Events.transfers evs) ~actual:(Events.transfers r.events)
          ~offset
      in
      let ei = Events.instructions evs and ai = Events.instructions r.events in
      let d =
        d
        @
        if List.length ei <> List.length ai then
          [
            Printf.sprintf "%d instructions expected, %d observed" (List.length ei) (List.length ai);
          ]
        else
          List.concat
            (List.map2
               (fun (e : Instr.t) (a : Instr.t) ->
                 if e.text <> a.text || e.bytes <> a.bytes || e.pc <> a.pc then
                   [
                     Printf.sprintf "instruction %d: %s, observed %s" e.idx
                       (Option.value e.text ~default:"?")
                       (Option.value a.text ~default:"?");
                   ]
                 else [])
               ei ai)
      in
      if d = [] then
        Printf.printf "%s: %d transfers and %d instructions equal the hand trace %s\n" trace
          (List.length (Events.transfers r.events))
          (List.length ai) hand
      else begin
        List.iter prerr_endline d;
        exit 1
      end

let k4_cmd =
  Cmd.v
    (Cmd.info "k4"
       ~doc:"Run a K4 hand trace through the models; with --against compare with a capture.")
    Term.(
      const k4 $ manifest_arg
      $ Arg.(required & opt (some file) None & info [ "hand" ] ~docv:"FILE" ~doc:"K4 pin trace.")
      $ Arg.(
          value
          & opt (some file) None
          & info [ "against" ] ~docv:"FST" ~doc:"Compare with this waveform."))

let replay dir =
  match Export.replay ~dir with
  | Error e -> die "%s" e
  | Ok [] ->
      Printf.printf
        "%s: replay reproduces events.jsonl, events.txt and perfetto.json byte for byte\n" dir
  | Ok l ->
      Printf.eprintf "%s: replay differs in %s\n" dir (String.concat ", " l);
      exit 1

let replay_cmd =
  Cmd.v
    (Cmd.info "replay" ~doc:"Observe an exported trace again and compare the bytes.")
    Term.(const replay $ Arg.(required & pos 0 (some dir) None & info [] ~docv:"DIR"))

let () =
  exit
    (Cmd.eval (Cmd.group (Cmd.info "i8080_observe") [ observe_cmd; check_cmd; k4_cmd; replay_cmd ]))
