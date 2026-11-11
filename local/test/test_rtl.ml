open I8080_trace
open I8080_rtl
open Helpers
module Manifest = I8080_trace.Manifest
module Events = I8080_trace.Events
module Observe = I8080_trace.Observe
module Frame = I8080_trace.Frame
module K4 = I8080_trace.K4
module Pipeline = I8080_trace.Pipeline
module Export = I8080_trace.Export
module Lockstep = I8080_trace.Lockstep

let run_program ?tick_every ?halt_stop ?max_tstates ?(stim = Fixture.nowait) program =
  let memory = Harness.new_memory () in
  Harness.load_program memory ~origin:0 program;
  let stim = match Stimulus.parse stim with Ok s -> s | Error e -> failwith e in
  Harness.run ?tick_every ?halt_stop ?max_tstates ~stim ~memory ()

let writes (r : Harness.result) =
  String.concat ","
    (List.map (fun (w : Harness.write) -> Printf.sprintf "%02x@%04x" w.data w.addr) r.writes)

let counted (r : Harness.result) = match r.counted with Some c -> string_of_int c | None -> "none"

let against hand (r : Harness.result) =
  match K4.compare_pins ~expected:(frames_of hand) ~actual:(I8080_rtl_trace.Rtl_trace.frames r) with
  | [] -> print_endline "pins equal the hand trace"
  | l -> List.iter print_endline l

let%expect_test "no waits: 27 T-states, one write, the K4 pin trace" =
  let r = Fixture.run Fixture.nowait in
  Printf.printf "counted=%s writes=%s\n" (counted r) (writes r);
  against Traces.nowait r;
  [%expect {|
    counted=27 writes=2a@2000
    pins equal the hand trace |}]

let%expect_test "two waits on the STA write: 29 T-states, still one write" =
  let r = Fixture.run Fixture.wait2 in
  Printf.printf "counted=%s writes=%s\n" (counted r) (writes r);
  against Traces.wait2 r;
  [%expect {|
    counted=29 writes=2a@2000
    pins equal the hand trace |}]

let%expect_test "the halt cycle address is the one after HLT, as in the hand trace" =
  let r = Fixture.run Fixture.nowait in
  let halt = List.find (fun (s : Harness.sample) -> s.tstate = Core.twh) r.samples in
  Printf.printf "TWH a=%04x pc=%04x\n" halt.a halt.pc;
  [%expect {| TWH a=0006 pc=0006 |}]

let%expect_test "a faster implementation clock gives the same T-state samples" =
  let base = Harness.pin_trace (Fixture.run Fixture.wait2) in
  List.iter
    (fun n ->
      let r = Fixture.run ~tick_every:n Fixture.wait2 in
      Printf.printf "tick every %d: %s\n" n
        (if Harness.pin_trace r = base then "same pin trace" else "DIFFERENT"))
    [ 2; 3; 5 ];
  [%expect
    {|
    tick every 2: same pin trace
    tick every 3: same pin trace
    tick every 5: same pin trace |}]

let%expect_test "A is 2a after MVI and the registers hold their reset values" =
  let r = Fixture.run Fixture.nowait in
  let at n = List.find (fun (s : Harness.sample) -> s.n - r.pre_sync = n) r.samples in
  List.iter
    (fun n -> Printf.printf "n=%d acc=%02x pc=%04x\n" n (at n).acc (at n).pc)
    [ 0; 6; 7; 26 ];
  [%expect
    {|
    n=0 acc=00 pc=0000
    n=6 acc=00 pc=0002
    n=7 acc=2a pc=0002
    n=26 acc=2a pc=0006 |}]

(* One word per T-state from the first SYNC: the status word at T1, r/w for a read or a write strobe,
   W for WAIT, - otherwise; ^ marks HLDA and e INTE. *)
let strip ?halt_stop ?(max_tstates = 60) ~stim program =
  let r = run_program ?halt_stop ~max_tstates ~stim:("reset 0 @4\n" ^ stim) program in
  let words =
    List.filter_map
      (fun (s : Harness.sample) ->
        if s.n < r.pre_sync then None
        else
          let w =
            if s.sync then Printf.sprintf "%02x" (Option.value s.dout ~default:0)
            else if s.dbin then "r"
            else if not s.wr_n then "w"
            else if s.wait then "W"
            else "-"
          in
          Some (w ^ (if s.hlda then "^" else "") ^ if s.inte then "e" else ""))
      r.samples
  in
  print_endline (String.concat " " words)

let%expect_test "HOLD: HLDA with T3 of a read and after T3 of a write, then idle until HOLD falls" =
  (* MVI A,2a; STA 2000; HLT. HOLD in T2 of the first fetch, in T2 of the STA write, and for one
     T-state only (HLDA for one T-state, no idle) *)
  strip ~stim:"hold 1 @6\nhold 0 @12\n" Fixture.program;
  strip ~stim:"hold 1 @23\nhold 0 @29\n" Fixture.program;
  strip ~stim:"hold 1 @6\nhold 0 @7\n" Fixture.program;
  (* in T1 or with READY low it is not taken; in the halt state HLDA follows HOLD *)
  strip ~stim:"hold 1 @5\nhold 0 @6\n" Fixture.program;
  strip ~stim:"ready 0 @6\nready 1 @9\nhold 1 @6\nhold 0 @8\n" Fixture.program;
  strip ~halt_stop:false ~max_tstates:20 ~stim:"hold 1 @12\nhold 0 @15\n" "\x76";
  [%expect
    {|
    a2 r -^ -^ -^ -^ -^ -^ 82 r - a2 r - - 82 r - 82 r - 00 - w a2 r - - 8a - W W W
    a2 r - - 82 r - a2 r - - 82 r - 82 r - 00 - w -^ -^ -^ -^ -^ a2 r - - 8a - W W W
    a2 r -^ - 82 r - a2 r - - 82 r - 82 r - 00 - w a2 r - - 8a - W W W
    a2 r - - 82 r - a2 r - - 82 r - 82 r - 00 - w a2 r - - 8a - W W W
    a2 r r r r - - 82 r - a2 r - - 82 r - 82 r - 00 - w a2 r - - 8a - W W W
    a2 r - - 8a - W W W^ W^ W^ W W W W
    |}]

let%expect_test "INT: acknowledged after the instruction that follows EI, PC kept, INTE cleared" =
  (* EI; NOP; NOP..., RST 7 supplied by INTA: 23, the return address 0002 pushed, then 0038 *)
  let nops = "\xfb" ^ String.make 0x40 '\x00' in
  strip ~max_tstates:32 ~stim:"inta ff\nint 1 @m0+3\nint 0 @m2+1\n" nops;
  (* a CALL through INTA: its address bytes come with 03 *)
  strip ~max_tstates:36 ~stim:"inta cd 20 00\nint 1 @m1+1\nint 0 @m2+1\n" nops;
  (* not in the last T-state: missed. With INTE clear, or after DI: ignored *)
  strip ~max_tstates:16 ~stim:"inta ff\nint 1 @m1+1\nint 0 @m1+2\n" nops;
  strip ~max_tstates:16 ~stim:"inta ff\nint 1 @m1+1\n" (String.make 8 '\x00');
  strip ~max_tstates:16 ~stim:"inta ff\nint 1 @m0+1\n" "\xfb\xf3\x00\x00\x00";
  (* out of a halt: two more T-states, then the acknowledge cycle 2b *)
  strip ~halt_stop:false ~max_tstates:34 ~stim:"inta ff\nint 1 @m2+3\nint 0 @m3+1\n" "\xfb\x76";
  (* not while a HOLD is being acknowledged: the next instruction runs first *)
  strip ~max_tstates:36 ~stim:"inta ff\nint 1 @m1+1\nhold 1 @10\nhold 0 @14\n" nops;
  [%expect
    {|
    a2 r - - a2e re -e -e 23 r - - - 04 - w 04 - w a2 r - - a2 r - -
    a2 r - - a2e re -e -e 23 r - - - 03 r - 03 r - 04 - w 04 - w a2 r - - a2 r
    a2 r - - a2e re -e -e a2e re -e
    a2 r - - a2 r - - a2 r -
    a2 r - - a2e re -e -e a2 r -
    a2 r - - a2e re -e -e 8ae -e We We We We 2b r - - - 04 - w 04 - w a2 r - -
    a2 r - - a2e re -^e -^e -^e -^e a2e re -e -e 23 r - - - 04 - w 04 - w a2 r - - a2 r
    |}]

let%expect_test "RESET during a run restarts at 0000" =
  let stim = {|reset 0 @4
reset 1 @14
reset 0 @17
|} in
  let r = run_program ~stim Fixture.program in
  let fetches =
    List.filter_map
      (fun (s : Harness.sample) -> if s.sync && not s.reset then Some (s.n, s.a) else None)
      r.samples
  in
  List.iter
    (fun (n, a) -> Printf.printf "T1 at sample %d a=%04x\n" n a)
    (List.filteri (fun i _ -> i < 6) fetches);
  Printf.printf "writes=%s\n" (writes r);
  [%expect
    {|
    T1 at sample 5 a=0000
    T1 at sample 9 a=0001
    T1 at sample 12 a=0002
    T1 at sample 18 a=0000
    T1 at sample 22 a=0001
    T1 at sample 25 a=0002
    writes=2a@2000
    |}]

let%expect_test "malformed stimulus statements are refused, io and inta are read" =
  List.iter
    (fun text ->
      match Stimulus.parse text with Ok _ -> print_endline "accepted" | Error e -> print_endline e)
    [
      "io 10 41 42\n";
      {|io 10 43
io 20 1
inta e7 cd
|};
      "io zz 1\n";
      "inta 1ff\n";
      "ready 2 @3\n";
      "ready 0 @x\n";
      "clock 0 @3\n";
      "# only a comment\n";
    ];
  [%expect
    {|
    accepted
    accepted
    line 1: bad io statement
    line 1: bad inta byte
    line 1: value "2" is not 0 or 1
    line 1: bad <when> "@x"
    line 1: unsupported statement "clock"
    accepted
    |}]

let%expect_test "the core elaborates, with its observation ports named" =
  let module Circ = Hardcaml.Circuit.With_interface (Core.I) (Core.O) in
  let c = Circ.create_exn ~name:"i8080_core" Core.create in
  let ports l =
    String.concat " "
      (List.sort compare
         (List.map
            (fun s ->
              Printf.sprintf "%s:%d" (List.hd (Hardcaml.Signal.names s)) (Hardcaml.Signal.width s))
            l))
  in
  Printf.printf "in: %s\nout: %s\n"
    (ports (Hardcaml.Circuit.inputs c))
    (ports (Hardcaml.Circuit.outputs c));
  let b = Buffer.create 4096 in
  Hardcaml.Rtl.output ~output_mode:(To_buffer b) Verilog c;
  Printf.printf "verilog: %b\n" (Str_find.contains (Buffer.contents b) "module i8080_core");
  [%expect
    {|
    in: clock:1 di:8 hold:1 int_req:1 ready:1 rst_in:1 tick:1
    out: a:16 ac:1 acc:8 accept:1 bc:16 c:1 cycle:3 dbin:1 de:16 dout:8 dout_en:1 hl:16 hlda:1 inte:1 opcode:8 p:1 pc:16 s:1 sample:1 sp:16 sync:1 tstate:3 wait_:1 wr_n:1 wz:16 z:1
    verilog: true
    |}]

(* P3: the waveform Vcd.wrap writes, read back with hwfrp through the observation library. *)
let hardcaml_manifest () =
  match Manifest.load "../manifest.json" with Ok m -> m | Error e -> failwith e

let vcd_of ?tick_every stim =
  let file = Filename.temp_file "i8080" ".vcd" in
  let oc = open_out_bin file in
  let r = Fixture.run ?tick_every ~vcd:oc stim in
  close_out oc;
  (file, r)

let observe ?rule m file =
  let m =
    match rule with
    | None -> m
    | Some rule -> { m with Manifest.clock = { m.Manifest.clock with Manifest.rule } }
  in
  match Observe.run ~frames:true m (Hwfrp_native.open_file file) with
  | Ok r -> r
  | Error l -> failwith (String.concat "\n" l)

let first_t1 (r : Observe.result) = Option.value (K4.first_t1 r.events) ~default:0

let%expect_test "hwfrp decodes the Vcd.wrap waveform of the fixture" =
  let file, _ = vcd_of Fixture.nowait in
  let r = observe (hardcaml_manifest ()) file in
  print r.events;
  [%expect
    {|
    reset epoch=0 at=0
    xfer fetch       a=0000 d=3e T5-7 waits=0
    xfer mem_read    a=0001 d=2a T9-11 waits=0
    xfer fetch       a=0002 d=32 T12-14 waits=0
    instr #0 0000: mvi a, 0x2a [3e 2a] T5-11 7T a:00->2a pc:0000->0002
    xfer mem_read    a=0003 d=00 T16-18 waits=0
    xfer mem_read    a=0004 d=20 T19-21 waits=0
    xfer mem_write   a=2000 d=2a T22-24 waits=0
    xfer fetch       a=0005 d=76 T25-27 waits=0
    instr #1 0002: sta 0x2000 [32 00 20] T12-24 13T pc:0002->0005
    xfer halt_ack    a=0006 d=~ T29-31 waits=0
    instr #2 0005: hlt [76] T25-31 7T pc:0005->0006
    halt-evidence first=29 n=23:a=2000,pc=0005 n=24:a=2000,pc=0005 n=25:a=0005,pc=0005 n=26:a=0005,pc=0006 n=27:a=0005,pc=0006 n=28:a=0005,pc=0006 n=29:a=0006,pc=0006 n=30:a=0006,pc=0006 n=31:a=0006,pc=0006 n=32:a=0006,pc=0006 n=33:a=0006,pc=0006
    end tstates=34 first_t1=5
    |}]

(* the decoded events of the hand-written K4 trace and of the waveform: kinds, T-states, waits,
   addresses and data *)
let%expect_test "the waveform events are the K4 events" =
  List.iter
    (fun (name, stim, hand) ->
      let file, _ = vcd_of stim in
      let r = observe (hardcaml_manifest ()) file in
      let expected =
        Pipeline.run (hardcaml_manifest ()).snapshot (List.to_seq (frames_of hand))
        |> Events.transfers
      in
      let diffs =
        K4.compare_transfers ~expected ~actual:(Events.transfers r.events) ~offset:(first_t1 r)
      in
      Printf.printf "%s: %d transfers, %d instructions, %s\n" name
        (List.length (Events.transfers r.events))
        (List.length (Events.instructions r.events))
        (if diffs = [] then "equal to the hand trace" else String.concat "; " diffs))
    [ ("nowait", Fixture.nowait, Traces.nowait); ("wait2", Fixture.wait2, Traces.wait2) ];
  [%expect
    {|
    nowait: 8 transfers, 3 instructions, equal to the hand trace
    wait2: 8 transfers, 3 instructions, equal to the hand trace
    |}]

let%expect_test "every sample of the waveform equals the harness sample" =
  List.iter
    (fun (name, stim) ->
      let file, run = vcd_of stim in
      let r = observe (hardcaml_manifest ()) file in
      let t1 = first_t1 r in
      let frames = List.filter (fun (f : Frame.t) -> f.n >= t1) r.frames in
      let diffs = K4.compare_pins ~expected:(I8080_rtl_trace.Rtl_trace.frames run) ~actual:frames in
      Printf.printf "%s: %d samples, %s\n" name (List.length frames)
        (if diffs = [] then "equal" else String.concat "; " diffs))
    [ ("nowait", Fixture.nowait); ("wait2", Fixture.wait2) ];
  [%expect {|
    nowait: 29 samples, equal
    wait2: 31 samples, equal
    |}]

let%expect_test "an undriven data bus is unknown in the waveform, not zero" =
  let file, _ = vcd_of Fixture.nowait in
  let r = observe (hardcaml_manifest ()) file in
  let f n = List.find (fun (f : Frame.t) -> f.n = n) r.frames in
  let show (f : Frame.t) =
    match f.dout with Some v -> Printf.sprintf "%02x" v | None -> "unknown"
  in
  let t1 = first_t1 r in
  Printf.printf "T1 %s, T2 %s, write T2 %s\n" (show (f t1)) (show (f (t1 + 1))) (show (f (t1 + 18)));
  [%expect {| T1 a2, T2 unknown, write T2 2a |}]

let%expect_test "events do not depend on the implementation clock" =
  let jsonl stim n =
    let file, _ = vcd_of ~tick_every:n stim in
    let m = hardcaml_manifest () in
    Export.jsonl m (observe m file).events
  in
  List.iter
    (fun n -> Printf.printf "tick every %d: %b\n" n (jsonl Fixture.wait2 n = jsonl Fixture.wait2 1))
    [ 2; 3; 7 ];
  [%expect {|
    tick every 2: true
    tick every 3: true
    tick every 7: true
    |}]

(* K2 rule 5 assumed Before_rising; Vcd.wrap writes the pre-edge values of cycle k at the timestamp of
   the edge that ends it, so "before" reads the cycle before. *)
let%expect_test "Before_rising reads the previous cycle of a Vcd.wrap waveform" =
  let file, _ = vcd_of Fixture.nowait in
  let m = hardcaml_manifest () in
  let after = observe m file and before = observe ~rule:Manifest.Before_rising m file in
  Printf.printf "after_rising:  %d transfers, %d instructions\n"
    (List.length (Events.transfers after.events))
    (List.length (Events.instructions after.events));
  Printf.printf "before_rising: %d transfers, %d instructions, first sample %s\n"
    (List.length (Events.transfers before.events))
    (List.length (Events.instructions before.events))
    (Format.asprintf "%a" Frame.pp (List.hd before.frames));
  let diffs = K4.compare_pins ~expected:after.frames ~actual:before.frames in
  Printf.printf "samples differ: %b\n" (diffs <> []);
  [%expect
    {|
    after_rising:  8 transfers, 3 instructions
    before_rising: 8 transfers, 3 instructions, first sample 0 0000 ~~ 00 0 0 1 0 0 0 1 0 0 1
    samples differ: true
    |}]

let%expect_test "a manifest naming a path the waveform lacks is refused" =
  let file, _ = vcd_of Fixture.nowait in
  let m = hardcaml_manifest () in
  let m =
    {
      m with
      signals =
        List.map
          (fun (s : Manifest.signal) ->
            if s.name = "wait" then { s with path = [ "outputs"; "waitt" ] }
            else if s.name = "pc" then { s with path = [ "outputs"; "pcc" ]; width = 16 }
            else s)
          m.signals;
    }
  in
  (match Observe.run m (Hwfrp_native.open_file file) with
  | Ok _ -> print_endline "accepted"
  | Error l -> List.iter print_endline l);
  [%expect {|
    missing outputs.waitt: wait
    missing outputs.pcc: pc
    |}]

let%expect_test "export and replay of the waveform are byte exact" =
  let file, _ = vcd_of Fixture.wait2 in
  let m = hardcaml_manifest () in
  let r = observe m file in
  let dir = Filename.temp_file "i8080" ".export" in
  Sys.remove dir;
  (match Export.write ~dir m ~trace:file r.events with Ok () -> () | Error e -> failwith e);
  (match Export.replay ~dir with
  | Ok [] -> print_endline "replay: identical"
  | Ok l -> Printf.printf "replay differs: %s\n" (String.concat "," l)
  | Error e -> print_endline e);
  [%expect {| replay: identical |}]

(* The model of goal 9 (Rivet's behavioural model) against the Hardcaml core, instruction by instruction. *)
let lockstep_report ~model_program =
  let file, _ = vcd_of Fixture.nowait in
  let m = hardcaml_manifest () in
  let r = observe m file in
  let setup =
    { Lockstep.memory = [ (0, model_program) ]; pc = 0; sp = 0; known = []; start_regs = None }
  in
  let times = Hashtbl.create 64 in
  List.iter (fun (f : Frame.t) -> Hashtbl.replace times f.n f.time) r.frames;
  Lockstep.run setup ~time_of:(Hashtbl.find_opt times) (Events.instructions r.events)

let%expect_test
    "the Hardcaml core agrees with the behavioural model, a wrong model program does not" =
  let show (r : Lockstep.report) =
    Printf.printf "%d agree, %d without registers, halted=%b%s\n" r.compared r.partial r.halted
      (match r.divergence with
      | None -> ""
      | Some d -> Format.asprintf "\n%a" Lockstep.pp_divergence d)
  in
  show (lockstep_report ~model_program:Fixture.program);
  show (lockstep_report ~model_program:"\x3e\x2b\x32\x00\x20\x76");
  [%expect
    {|
    3 agree, 0 without registers, halted=true
    0 agree, 0 without registers, halted=false
    instruction 0 at 0000: bytes: observed 3e 2a, model 3e 2b (T5-11, waveform time 60-120)
    |}]
