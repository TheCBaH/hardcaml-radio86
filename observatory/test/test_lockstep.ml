open I8080_trace

(* MVI A,2a / STA 2000 / HLT as the instruction model would report them from the pins. *)
let cycle seq kind addr data first last : Bus.cycle =
  { epoch = 0; seq; kind; status = None; addr = Some addr; data; first; last; waits = 0 }

let regs ?(a = 0) ?(pc = 0) () = Regs.of_ints ~a ~b:0 ~c:0 ~d:0 ~e:0 ~h:0 ~l:0 ~f:2 ~sp:0xfe00 ~pc

let instr idx pc bytes accesses first last before after : Instr.t =
  {
    idx;
    epoch = 0;
    pc = Some pc;
    bytes;
    expected_len = Some (List.length bytes);
    text = None;
    first;
    last;
    tstates = last - first + 1;
    accesses;
    before = Some before;
    after = Some after;
    complete = true;
    why = "";
  }

let program ?(a1 = 0x2a) ?(wdata = 0x2a) () =
  [
    instr 0 0x100 [ 0x3e; 0x2a ]
      [ cycle 0 Bus.Fetch 0x100 (Some 0x3e) 0 2; cycle 1 Bus.Mem_read 0x101 (Some 0x2a) 4 6 ]
      0 6 (regs ~pc:0x100 ()) (regs ~a:a1 ~pc:0x102 ());
    instr 1 0x102 [ 0x32; 0x00; 0x20 ]
      [
        cycle 2 Bus.Fetch 0x102 (Some 0x32) 7 9;
        cycle 3 Bus.Mem_read 0x103 (Some 0) 11 13;
        cycle 4 Bus.Mem_read 0x104 (Some 0x20) 14 16;
        cycle 5 Bus.Mem_write 0x2000 (Some wdata) 17 19;
      ]
      7 19 (regs ~a:a1 ~pc:0x102 ()) (regs ~a:a1 ~pc:0x105 ());
    instr 2 0x105 [ 0x76 ]
      [ cycle 6 Bus.Fetch 0x105 (Some 0x76) 20 22; cycle 7 Bus.Halt_ack 0x105 None 24 26 ]
      20 26 (regs ~a:a1 ~pc:0x105 ()) (regs ~a:a1 ~pc:0x106 ());
  ]

let setup =
  {
    Lockstep.memory = [ (0x100, "\x3e\x2a\x32\x00\x20\x76") ];
    pc = 0x100;
    sp = 0xfe00;
    known = [];
    start_regs = None;
  }

let show (r : Lockstep.report) =
  Printf.printf "agree=%d halted=%b%s\n" r.compared r.halted
    (match r.divergence with
    | None -> ""
    | Some d -> Format.asprintf "\n%a" Lockstep.pp_divergence d)

let%expect_test "the fixture agrees with the model" =
  show (Lockstep.run setup ~time_of:(fun n -> Some (Int64.of_int (n * 14))) (program ()));
  [%expect {| agree=3 halted=true |}]

let%expect_test "a wrong register is the first divergence, with its T-states and times" =
  show (Lockstep.run setup ~time_of:(fun n -> Some (Int64.of_int (n * 14))) (program ~a1:0x2b ()));
  [%expect
    {|
    agree=0 halted=false
    instruction 0 at 0100: reg a: observed 2b, model 2a (T0-6, waveform time 0-84) |}]

let%expect_test "a wrong written byte" =
  show (Lockstep.run setup ~time_of:(fun _ -> None) (program ~wdata:0x2b ()));
  [%expect
    {|
    agree=1 halted=false
    instruction 1 at 0102: accesses: observed [write 2000=2b], model [write 2000=2a] (T7-19) |}]

let%expect_test "an observation that started at the wrong address" =
  show (Lockstep.run { setup with pc = 0x200 } ~time_of:(fun _ -> None) (program ()));
  [%expect
    {|
    agree=0 halted=false
    instruction -1: capture: no observed instruction starts at 0200 (T0-0) |}]

let%expect_test
    "XTHL's writes in the other order: unclassified stops the run, classified is counted" =
  let xthl =
    instr 0 0x100 [ 0xe3 ]
      [
        cycle 0 Bus.Fetch 0x100 (Some 0xe3) 0 2;
        cycle 1 Bus.Stack_read 0xfe00 (Some 0) 5 7;
        cycle 2 Bus.Stack_read 0xfe01 (Some 0) 8 10;
        cycle 3 Bus.Stack_write 0xfe01 (Some 0) 11 13;
        cycle 4 Bus.Stack_write 0xfe00 (Some 0) 14 16;
      ]
      0 17 (regs ~pc:0x100 ()) (regs ~pc:0x101 ())
  in
  let setup = { setup with memory = [ (0x100, "\xe3") ] } in
  show (Lockstep.run setup ~time_of:(fun _ -> None) [ xthl ]);
  let r =
    Lockstep.run
      { setup with known = [ (0xe3, "access order", "stack writes are high byte first") ] }
      ~time_of:(fun _ -> None)
      [ xthl ]
  in
  show r;
  List.iter
    (fun ((d : Lockstep.divergence), note) -> Printf.printf "classified %s: %s\n" d.what note)
    r.classified;
  [%expect
    {|
    agree=0 halted=false
    instruction 0 at 0100: access order: observed [read fe00=00; read fe01=00; write fe01=00; write fe00=00], model [read fe00=00; read fe01=00; write fe00=00; write fe01=00] (T0-17)
    agree=1 halted=false
    classified access order: stack writes are high byte first |}]
