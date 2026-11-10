(* The Hardcaml core's Vcd.wrap waveform, registers included, read back with hwfrp: the executed
   instructions and the state they update. *)
open I8080_trace

let show (r : Observe.result) =
  let steps = Trail.of_events r.events in
  List.iter print_endline (Trail.lines steps);
  print_endline "--";
  List.iter print_endline (Trail.summary (Trail.final steps))

let rtl_trail stim =
  let file = Filename.temp_file "i8080" ".vcd" in
  let oc = open_out_bin file in
  ignore (Fixture.run ~vcd:oc stim);
  close_out oc;
  let m = match Manifest.load "../manifest.json" with Ok m -> m | Error e -> failwith e in
  match Observe.run m (Hwfrp_native.open_file file) with
  | Ok r -> r
  | Error l -> failwith (String.concat "\n" l)

let%expect_test "the Hardcaml core: MVI, STA and HLT, with and without waits" =
  show (rtl_trail Fixture.nowait);
  print_endline "== two waits on the write";
  show (rtl_trail Fixture.wait2);
  [%expect
    {|
    0000 mvi a, 0x2a      a:00->2a pc:0000->0002
    0002 sta 0x2000       pc:0002->0005 mem[2000]:?->2a
    0005 hlt              pc:0005->0006
    --
    3 instructions
    regs a=2a b=00 c=00 d=00 e=00 h=00 l=00 f=02 sp=0000 pc=0006
    mem 0000: 3e 2a 32 00 20 76
    mem 2000: 2a
    == two waits on the write
    0000 mvi a, 0x2a      a:00->2a pc:0000->0002
    0002 sta 0x2000       pc:0002->0005 mem[2000]:?->2a
    0005 hlt              pc:0005->0006
    --
    3 instructions
    regs a=2a b=00 c=00 d=00 e=00 h=00 l=00 f=02 sp=0000 pc=0006
    mem 0000: 3e 2a 32 00 20 76
    mem 2000: 2a
    |}]
