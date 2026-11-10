(* Finding the probes of a manifest in a trace from its declarations (ovcd's signal list), and what the
   binding pass and the run report about a trace. *)
open I8080_trace
open Helpers

let m_flat () = with_regs ()
let frames = lazy (Model_trace.frames Test_trail.sum_loop)
let trace ?drop () = Synth.trace ?drop ~regs:true (m_flat ()) (Lazy.force frames)

(* the manifest's paths moved: a CPU under another scope, signals named without the pin_ prefix *)
let moved (m : Manifest.t) =
  let re path = "top" :: "cpu" :: [ List.nth path (List.length path - 1) ] in
  { m with signals = List.map (fun (s : Manifest.signal) -> { s with path = re s.path }) m.signals }

let%expect_test "the declarations of a trace as ovcd's JSON lines" =
  List.iteri
    (fun i s -> if i < 3 || i >= 25 then print_endline (Vcd_signals.to_line s))
    (Discover.signals (trace ()));
  [%expect
    {|
    {"path":"i8080_tb.pin_a","parts":["i8080_tb","pin_a"],"scope_types":["module"],"id":"s0","width":16,"var_type":"wire"}
    {"path":"i8080_tb.pin_dout","parts":["i8080_tb","pin_dout"],"scope_types":["module"],"id":"s1","width":8,"var_type":"wire"}
    {"path":"i8080_tb.pin_din","parts":["i8080_tb","pin_din"],"scope_types":["module"],"id":"s2","width":8,"var_type":"wire"}
    {"path":"i8080_tb.clk","parts":["i8080_tb","clk"],"scope_types":["module"],"id":"clk","width":1,"var_type":"wire"}
    {"path":"i8080_tb.f2","parts":["i8080_tb","f2"],"scope_types":["module"],"id":"f2","width":1,"var_type":"wire"}
    |}]

let%expect_test "probes that are where the manifest says need no hint" =
  let sigs = Discover.signals (trace ()) in
  let n = List.length (Discover.check (m_flat ()) sigs) in
  let present =
    List.length (List.filter (fun (_, f) -> f = Discover.Present) (Discover.check (m_flat ()) sigs))
  in
  Printf.printf "%d probes, %d present\n" n present;
  [%expect {| 27 probes, 27 present |}]

let%expect_test "a manifest whose scope moved: hints, re-pathing, then a run" =
  let tr = trace () in
  let sigs = Discover.signals tr in
  let m = moved (m_flat ()) in
  (match Observe.bind_check m tr with
  | Ok () -> print_endline "bound"
  | Error l -> Printf.printf "%d probes unbound, first: %s\n" (List.length l) (List.hd l));
  List.iter
    (fun (p, _) -> Option.iter print_endline (Discover.hint sigs p))
    (List.filteri (fun i _ -> i < 3) (Discover.check m sigs));
  (match Discover.repath m sigs with
  | Error l -> List.iter print_endline l
  | Ok m' -> (
      match Observe.run m' tr with
      | Error l -> List.iter print_endline l
      | Ok r ->
          Printf.printf "re-pathed: %d T-states, %d instructions\n" r.tstates
            (List.length (Events.instructions r.events))));
  [%expect
    {|
    25 probes unbound, first:   missing top.cpu.pin_a: a
    a: declared as i8080_tb.pin_a
    re-pathed: 104 T-states, 15 instructions
    |}]

let%expect_test "a signal the trace lacks is named with the nearest declarations" =
  let tr = trace ~drop:[ "hold" ] () in
  let sigs = Discover.signals tr in
  let m = m_flat () in
  List.iter
    (fun (p, f) ->
      if f <> Discover.Present then
        print_endline
          (Option.value (Discover.hint sigs p) ~default:(p.Discover.name ^ ": nothing like it")))
    (Discover.check m sigs);
  (match Discover.repath m sigs with
  | Ok _ -> print_endline "re-pathed"
  | Error l -> List.iter print_endline l);
  [%expect {|
    hold: nothing like it
    hold: i8080_tb.pin_hold is not declared
    |}]

let%expect_test "one binding pass names every probe that is missing, the clock too" =
  let m = m_flat () in
  let gone = function "a" | "int" | "sp" -> true | _ -> false in
  let m =
    {
      m with
      clock = { m.clock with path = [ "i8080_tb"; "nope" ] };
      signals =
        List.map
          (fun (s : Manifest.signal) ->
            if gone s.name then { s with path = [ "i8080_tb"; "gone_" ^ s.name ] } else s)
          m.signals;
    }
  in
  (match Observe.bind_check m (trace ()) with
  | Ok () -> print_endline "bound"
  | Error l -> List.iter print_endline l);
  [%expect
    {|
    missing nope: T-state clock
    missing i8080_tb.gone_a: a
    missing i8080_tb.gone_int: int
    missing i8080_tb.gone_sp: sp
    |}]

let%expect_test "a misspelt name gets the declarations that start like it" =
  let sigs = Discover.signals (trace ()) in
  let m = m_flat () in
  let m =
    {
      m with
      signals =
        List.map
          (fun (s : Manifest.signal) ->
            if s.name = "do" then { s with path = [ "i8080_tb"; "pin_dou" ] } else s)
          m.signals;
    }
  in
  List.iter
    (fun (p, f) -> if f <> Discover.Present then print_endline (Option.get (Discover.hint sigs p)))
    (Discover.check m sigs);
  [%expect {| do: nearest by name i8080_tb.pin_dout (8) |}]

let%expect_test "a run counts the enabled samples and summarises the signals" =
  match Observe.run (m_flat ()) (trace ()) with
  | Error l -> List.iter print_endline l
  | Ok r ->
      Printf.printf "tstates=%d enabled=%d\n" r.tstates r.enabled;
      List.iter
        (fun (d : Hwfrp.Diag.t) ->
          Printf.printf "%s %s %d signals\n" d.code
            (match d.severity with
            | Hwfrp.Diag.Info -> "info"
            | Warning -> "warning"
            | Error -> "error")
            (List.length d.paths))
        r.diagnostics;
      [%expect
        {|
    tstates=104 enabled=104
    signal.constant info 11 signals
    signal.unknown-at-end warning 3 signals
    |}]
