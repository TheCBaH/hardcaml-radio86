let jsonl m evs =
  let b = Buffer.create 4096 in
  Buffer.add_string b (Events.header m);
  Buffer.add_char b '\n';
  List.iter
    (fun e ->
      Option.iter
        (fun l ->
          Buffer.add_string b l;
          Buffer.add_char b '\n')
        (Events.to_json e))
    evs;
  Buffer.contents b

let text evs =
  let b = Buffer.create 4096 in
  List.iter
    (fun e ->
      Option.iter
        (fun l ->
          Buffer.add_string b l;
          Buffer.add_char b '\n')
        (Events.to_text e))
    evs;
  Buffer.contents b

let read f = In_channel.with_open_bin f In_channel.input_all
let write_file f s = Out_channel.with_open_bin f (fun oc -> Out_channel.output_string oc s)
let ( // ) = Filename.concat

let write ~dir (m : Manifest.t) ~trace evs =
  try
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
    let data = read trace in
    let m =
      {
        m with
        trace =
          Some
            {
              file = "trace.fst";
              format = "fst";
              sha256 = Sha256.hex data;
              bytes = String.length data;
            };
      }
    in
    write_file (dir // "trace.fst") data;
    write_file (dir // "manifest.json") (Manifest.to_string m);
    write_file (dir // "events.jsonl") (jsonl m evs);
    write_file (dir // "events.txt") (text evs);
    write_file (dir // "perfetto.json") (Perfetto.render m evs);
    Ok ()
  with Sys_error e -> Error e

let replay ~dir =
  match Manifest.load (dir // "manifest.json") with
  | Error e -> Error e
  | Ok m -> (
      match m.trace with
      | None -> Error "the exported manifest names no trace"
      | Some t -> (
          let path = dir // t.file in
          match read path with
          | exception Sys_error e -> Error e
          | data -> (
              if Sha256.hex data <> t.sha256 then
                Error
                  (Printf.sprintf "%s: SHA-256 %s, the manifest records %s" t.file (Sha256.hex data)
                     t.sha256)
              else
                match Observe.run m (Hwfrp_native.open_file path) with
                | Error l -> Error (String.concat "; " l)
                | Ok r ->
                    let want =
                      [
                        ("events.jsonl", jsonl m r.events);
                        ("events.txt", text r.events);
                        ("perfetto.json", Perfetto.render m r.events);
                      ]
                    in
                    Ok
                      (List.filter_map
                         (fun (f, s) ->
                           if (try read (dir // f) with Sys_error _ -> "") = s then None
                           else Some f)
                         want))))
