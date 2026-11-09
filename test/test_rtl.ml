open I8080_trace
open I8080_rtl
open Helpers

let run_program ?tick_every ?(stim = Fixture.nowait) program =
  let memory = Harness.new_memory () in
  Harness.load_program memory ~origin:0 program;
  let stim = match Stimulus.parse stim with Ok s -> s | Error e -> failwith e in
  Harness.run ?tick_every ~stim ~memory ()

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
    n=6 acc=2a pc=0002
    n=7 acc=2a pc=0002
    n=26 acc=2a pc=0006 |}]

let%expect_test "every opcode but MVI A, STA and HLT is rejected at its fetch" =
  let rejected = ref 0 and accepted = ref [] in
  for op = 0 to 255 do
    let r = run_program (String.make 1 (Char.chr op) ^ "\x00\x00") in
    match r.trapped with
    | Some (n, cause) when n - r.pre_sync = 3 ->
        assert (cause = Core.cause_opcode);
        assert (r.writes = []);
        incr rejected
    | _ -> accepted := op :: !accepted
  done;
  Printf.printf "implemented: %s\nrejected: %d\n"
    (String.concat " " (List.rev_map (Printf.sprintf "%02x") !accepted))
    !rejected;
  [%expect {|
    implemented: 32 3e 76
    rejected: 253
    |}]

let%expect_test "a rejected opcode stops the bus" =
  let r = run_program "\x3e\x2a\x00" in
  let last = List.nth r.samples (List.length r.samples - 1) in
  Printf.printf "trapped=%s last: sync=%b dbin=%b wr_n=%b trap=%b opcode-cycle=%d\n"
    (match r.trapped with
    | Some (n, c) -> Printf.sprintf "T%d cause %d" (n - r.pre_sync) c
    | None -> "no")
    last.sync last.dbin last.wr_n last.trap last.cycle;
  [%expect {| trapped=T10 cause 1 last: sync=false dbin=false wr_n=true trap=true opcode-cycle=0 |}]

let%expect_test "HOLD and INT are rejected, not ignored" =
  List.iter
    (fun (sigs, stim) ->
      let r = run_program ~stim:("reset 0 @4\n" ^ stim) Fixture.program in
      Printf.printf "%s: %s\n" sigs
        (match r.trapped with Some (_, c) -> "trap, cause " ^ string_of_int c | None -> "no trap"))
    [ ("hold", "hold 1 @m2+0\n"); ("int", "int 1 @m1+1\n") ];
  [%expect {|
    hold: trap, cause 2
    int: trap, cause 2 |}]

let%expect_test "RESET during a run restarts at 0000" =
  let r = run_program ~stim:"reset 0 @4\nreset 1 @14\nreset 0 @17\n" Fixture.program in
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

let%expect_test "stimulus statements the skeleton cannot honour are refused" =
  List.iter
    (fun text ->
      match Stimulus.parse text with Ok _ -> print_endline "accepted" | Error e -> print_endline e)
    [
      "io 0x10 1 2\n";
      "inta 0xff\n";
      "ready 2 @3\n";
      "ready 0 @x\n";
      "clock 0 @3\n";
      "# only a comment\n";
    ];
  [%expect
    {|
    line 1: unsupported statement "io"
    line 1: unsupported statement "inta"
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
    out: a:16 ac:1 acc:8 accept:1 bc:16 c:1 cycle:3 dbin:1 de:16 dout:8 dout_en:1 hl:16 hlda:1 inte:1 opcode:8 p:1 pc:16 s:1 sample:1 sp:16 sync:1 trap:1 trap_cause:2 tstate:3 wait_:1 wr_n:1 wz:16 z:1
    verilog: true
    |}]
