open I8080_trace

let pins_only (m : Manifest.t) =
  { m with signals = List.filter (fun (s : Manifest.signal) -> s.kind = Manifest.Pin) m.signals }

let frames_of text = match K4.parse text with Ok f -> f | Error e -> failwith e
let print evs = List.iter (fun e -> Option.iter print_endline (Events.to_text e)) evs
