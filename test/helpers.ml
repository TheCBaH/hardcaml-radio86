open I8080_trace

let manifest () =
  match Manifest.load "../manifests/vm80a-fst.json" with Ok m -> m | Error e -> failwith e

let pins_only (m : Manifest.t) =
  { m with signals = List.filter (fun (s : Manifest.signal) -> s.kind = Manifest.Pin) m.signals }

(* The synthetic waveform keeps every pin directly under [i8080_tb]. *)
let flat (m : Manifest.t) =
  {
    m with
    signals =
      List.map
        (fun (s : Manifest.signal) ->
          { s with path = [ "i8080_tb"; List.nth s.path (List.length s.path - 1) ] })
        m.signals;
  }

let frames_of text = match K4.parse text with Ok f -> f | Error e -> failwith e
let print evs = List.iter (fun e -> Option.iter print_endline (Events.to_text e)) evs

(* The vm80a manifest with its pins and registers directly under [i8080_tb], as Synth writes them; the
   [xchg] register is vm80a's own and Model_trace has no exchange state. *)
let with_regs () =
  let m = flat (manifest ()) in
  { m with signals = List.filter (fun (s : Manifest.signal) -> s.name <> "xchg") m.signals }
