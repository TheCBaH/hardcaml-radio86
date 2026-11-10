open I8080_trace
open Helpers

let events_of m frames =
  match Observe.run m (Synth.trace m frames) with
  | Ok r -> r
  | Error l -> failwith (String.concat "\n" l)

let%test "hwfrp over a synthetic waveform gives the events of the frames, both fixtures" =
  let m = flat (pins_only (manifest ())) in
  List.for_all
    (fun text ->
      let frames = frames_of text in
      let direct = Pipeline.run m.snapshot (List.to_seq frames) in
      let r = events_of m frames in
      r.tstates = List.length frames
      && List.map Events.to_json r.events = List.map Events.to_json direct)
    [ Traces.nowait; Traces.wait2 ]

let%expect_test "a missing probe is a hard error naming it" =
  let m = flat (pins_only (manifest ())) in
  let m =
    {
      m with
      signals =
        List.map
          (fun (s : Manifest.signal) ->
            if s.name = "wait" then { s with path = [ "i8080_tb"; "pin_waitt" ] } else s)
          m.signals;
    }
  in
  let tr = Synth.trace (flat (pins_only (manifest ()))) (frames_of Traces.nowait) in
  (match Observe.run m tr with
  | Ok _ -> print_endline "unexpectedly ran"
  | Error l -> List.iter print_endline l);
  [%expect {| missing i8080_tb.pin_waitt: wait |}]

let%expect_test "a width that differs is a hard error" =
  let m = flat (pins_only (manifest ())) in
  let m =
    {
      m with
      signals =
        List.map
          (fun (s : Manifest.signal) -> if s.name = "a" then { s with width = 8 } else s)
          m.signals;
    }
  in
  let tr = Synth.trace (flat (pins_only (manifest ()))) (frames_of Traces.nowait) in
  (match Observe.run m tr with
  | Ok _ -> print_endline "unexpectedly ran"
  | Error l -> List.iter print_endline l);
  [%expect {| width   i8080_tb.pin_a 16, expected 8: a |}]

let%expect_test "the trace lacking a pin the manifest names, and a missing clock" =
  let m = flat (pins_only (manifest ())) in
  let tr = Synth.trace ~drop:[ "hold"; "int" ] m (frames_of Traces.nowait) in
  (match Observe.bind_check m tr with
  | Ok () -> print_endline "ok"
  | Error l -> List.iter print_endline l);
  let m' = { m with clock = { m.clock with path = [ "i8080_tb"; "nope" ] } } in
  (match Observe.bind_check m' tr with
  | Ok () -> print_endline "ok"
  | Error l -> List.iter print_endline l);
  [%expect
    {|
    missing i8080_tb.pin_hold: hold
    missing i8080_tb.pin_int: int
    missing nope: T-state clock
    missing i8080_tb.pin_hold: hold
    missing i8080_tb.pin_int: int
    |}]
