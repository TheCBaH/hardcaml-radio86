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

let parse_known file =
  List.filter_map
    (fun line ->
      let line = String.trim line in
      if line = "" || line.[0] = '#' then None
      else
        match String.index_opt line ':' with
        | None -> die "%s: expected '<opcode> <kind>: <note>' in %S" file line
        | Some i -> (
            let note = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
            let head = String.sub line 0 i in
            match String.index_opt head ' ' with
            | Some j -> (
                match int_of_string_opt ("0x" ^ String.sub head 0 j) with
                | Some op ->
                    Some
                      (op, String.trim (String.sub head (j + 1) (String.length head - j - 1)), note)
                | None -> die "%s: bad opcode in %S" file line)
            | None -> die "%s: expected '<opcode> <kind>: <note>' in %S" file line))
    (String.split_on_char '\n' (read_file file))

let lockstep manifest trace program shim load pc sp plant expect_div all known =
  let m = load_manifest manifest in
  let plant =
    match Lockstep.plant_of_string plant with
    | Some p -> p
    | None -> die "unknown planted bug %s (none, ult, aux, carry)" plant
  in
  let r =
    match Observe.run ~frames:true m (open_trace trace) with
    | Error l -> die "%s" (String.concat "\n" l)
    | Ok r -> r
  in
  let times = Hashtbl.create 1024 in
  List.iter (fun (f : Frame.t) -> Hashtbl.replace times f.n f.time) r.frames;
  let setup =
    {
      Lockstep.memory =
        [
          (0, read_file (Filename.concat shim "shim-boot.bin"));
          (5, read_file (Filename.concat shim "shim-bdos.bin"));
          (0xfe00, read_file (Filename.concat shim "shim-handler.bin"));
          (load, read_file program);
        ];
      pc;
      sp;
      start_regs = None;
      known = (match known with Some f -> parse_known f | None -> []);
    }
  in
  let rep =
    Lockstep.run ~plant ~all setup ~time_of:(Hashtbl.find_opt times) (Events.instructions r.events)
  in
  Printf.printf
    "%d instructions agree (%d before %04x skipped, %d compared without registers, %d cut-short \
     fetches left out); model %d T-states%s\n"
    rep.compared rep.skipped pc rep.partial rep.tail rep.model_tstates
    (if rep.halted then ", halted" else "");
  (let seen = Hashtbl.create 4 in
   List.iter
     (fun ((d : Lockstep.divergence), note) ->
       Hashtbl.replace seen (d.what, note)
         (1 + Option.value (Hashtbl.find_opt seen (d.what, note)) ~default:0))
     rep.classified;
   Hashtbl.iter (fun (w, note) n -> Printf.printf "  classified: %d x %s (%s)\n" n w note) seen);
  if all then begin
    let counts = Hashtbl.create 8 in
    List.iter
      (fun (d : Lockstep.divergence) ->
        Hashtbl.replace counts d.what (1 + Option.value (Hashtbl.find_opt counts d.what) ~default:0))
      rep.all;
    Hashtbl.iter (fun k v -> Printf.printf "  %-14s %d\n" k v) counts;
    List.iteri (fun n d -> if n < 12 then Format.printf "  %a@." Lockstep.pp_divergence d) rep.all
  end;
  match (rep.divergence, expect_div) with
  | None, false -> ()
  | Some d, false ->
      Format.eprintf "DIVERGENCE %a@." Lockstep.pp_divergence d;
      exit 1
  | Some d, true -> Format.printf "divergence caught as expected: %a@." Lockstep.pp_divergence d
  | None, true ->
      prerr_endline "no divergence, but one was expected";
      exit 1

let lockstep_cmd =
  Cmd.v
    (Cmd.info "lockstep"
       ~doc:"Compare the observed instructions with Rivet's behavioural model, one by one.")
    Term.(
      const lockstep $ manifest_arg $ trace_arg
      $ Arg.(
          required
          & opt (some file) None
          & info [ "program" ] ~docv:"COM" ~doc:"The CP/M program that ran.")
      $ Arg.(
          required
          & opt (some dir) None
          & info [ "shim" ] ~docv:"DIR"
              ~doc:"Directory with shim-boot.bin, shim-bdos.bin and shim-handler.bin.")
      $ Arg.(value & opt int 0x100 & info [ "load" ] ~docv:"ADDR" ~doc:"Load address.")
      $ Arg.(value & opt int 0x100 & info [ "pc" ] ~docv:"ADDR" ~doc:"First instruction compared.")
      $ Arg.(value & opt int 0xfe00 & info [ "sp" ] ~docv:"ADDR" ~doc:"Initial SP.")
      $ Arg.(
          value & opt string "none"
          & info [ "plant" ] ~docv:"BUG" ~doc:"Plant a model bug (ult, aux, carry).")
      $ Arg.(
          value & flag & info [ "expect-divergence" ] ~doc:"Succeed only if a divergence is found.")
      $ Arg.(
          value & flag
          & info [ "all" ] ~doc:"Do not stop at the first divergence; count them by kind.")
      $ Arg.(
          value
          & opt (some file) None
          & info [ "known" ] ~docv:"FILE"
              ~doc:"Classified differences, one '<opcode> <kind>: <note>' per line."))

let frames manifest trace from until =
  let m = load_manifest manifest in
  let r =
    match Observe.run ~frames:true m (open_trace trace) with
    | Error l -> die "%s" (String.concat "\n" l)
    | Ok r -> r
  in
  let h w = function None -> String.make w '~' | Some v -> Printf.sprintf "%0*x" w v in
  print_endline "# n a do di sync dbin wr_n wait | acc bc de hl sp pc wz flags(zspcac)";
  List.iter
    (fun (f : Frame.t) ->
      if f.n >= from && f.n <= until then begin
        let b = function None -> "~" | Some true -> "1" | Some false -> "0" in
        Printf.printf "%d %s %s %s %s %s %s %s |" f.n (h 4 f.a) (h 2 f.dout) (h 2 f.din) (b f.sync)
          (b f.dbin) (b f.wr_n) (b f.wait);
        match f.regs with
        | Some r ->
            Printf.printf " %s %s %s %s %s %s %s %s%s%s%s%s%s\n" (h 2 r.acc) (h 4 r.bc) (h 4 r.de)
              (h 4 r.hl) (h 4 r.sp) (h 4 r.pc) (h 4 r.wz) (h 1 r.z) (h 1 r.s) (h 1 r.p) (h 1 r.c)
              (h 1 r.ac)
              (match r.swap with Frame.Fixed -> "" | Frame.Flag x -> " x" ^ h 1 x)
        | None -> print_newline ()
      end)
    r.frames

let frames_cmd =
  Cmd.v
    (Cmd.info "frames"
       ~doc:"Print the T-state samples (pins and registers) of a range, like a K4 trace.")
    Term.(
      const frames $ manifest_arg $ trace_arg
      $ Arg.(value & opt int 0 & info [ "from" ] ~docv:"N")
      $ Arg.(value & opt int max_int & info [ "to" ] ~docv:"N"))

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
    (Cmd.eval
       (Cmd.group (Cmd.info "i8080_observe")
          [ observe_cmd; check_cmd; k4_cmd; replay_cmd; lockstep_cmd; frames_cmd ]))
