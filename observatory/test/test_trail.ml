(* The executed instructions and the state they update, read through hwfrp from a synthetic waveform of
   programs run in Rivet's model, given the bus shape of the K4 traces. *)
open I8080_trace
open Helpers
open Reference

let observe ?(regs = true) frames =
  let m = if regs then with_regs () else pins_only (manifest ()) in
  match Observe.run m (Synth.trace ~regs m frames) with
  | Ok r -> r
  | Error l -> failwith (String.concat "\n" l)

let show (r : Observe.result) =
  let steps = Trail.of_events r.events in
  List.iter print_endline (Trail.lines steps);
  print_endline "--";
  List.iter print_endline (Trail.summary (Trail.final steps))

let hex l = String.concat "" (List.map (Printf.sprintf "%c") (List.map Char.chr l))

(* mvi b,3; xra a; loop: add b; dcr b; jnz loop; mov m,a; hlt, over 0x55 in memory *)
let sum_loop =
  hex [ 0x21; 0x00; 0x20; 0x36; 0x55; 0x06; 0x03; 0xaf; 0x80; 0x05; 0xc2; 0x08; 0x00; 0x77; 0x76 ]

(* push, call and ret on a stack at 0x3000, then in and out *)
let stack_io =
  hex
    [
      0x31;
      0x00;
      0x30;
      0x01;
      0x34;
      0x12;
      0xc5;
      0xcd;
      0x10;
      0x00;
      0xd1;
      0xdb;
      0x07;
      0xd3;
      0x08;
      0x76;
      0x3c;
      0xc9;
    ]

let%expect_test "a counting loop: registers and the byte it overwrites" =
  show (observe (Model_trace.frames sum_loop));
  [%expect
    {|
    0000 lxi h, 0x2000    h:00->20 pc:0000->0003
    0003 mvi m, 0x55      pc:0003->0005 mem[2000]:?->55
    0005 mvi b, 0x03      b:00->03 pc:0005->0007
    0007 xra a            f:02->46 pc:0007->0008
    0008 add b            a:00->03 f:46->06 pc:0008->0009
    0009 dcr b            b:03->02 f:06->12 pc:0009->000a
    000a jnz 0x0008       pc:000a->0008
    0008 add b            a:03->05 f:12->06 pc:0008->0009
    0009 dcr b            b:02->01 f:06->12 pc:0009->000a
    000a jnz 0x0008       pc:000a->0008
    0008 add b            a:05->06 f:12->06 pc:0008->0009
    0009 dcr b            b:01->00 f:06->56 pc:0009->000a
    000a jnz 0x0008       pc:000a->000d
    000d mov m, a         pc:000d->000e mem[2000]:55->06
    000e hlt              pc:000e->000f
    --
    15 instructions
    regs a=06 b=00 c=00 d=00 e=00 h=20 l=00 f=56 sp=3000 pc=000f
    mem 0000: 21 00 20 36 55 06 03 af 80 05 c2 08 00 77 76
    mem 2000: 06
    |}]

let%expect_test "the stack, a call and port transfers" =
  show (observe (Model_trace.frames ~sp:0 stack_io));
  [%expect
    {|
    0000 lxi sp, 0x3000   sp:0000->3000 pc:0000->0003
    0003 lxi b, 0x1234    b:00->12 c:00->34 pc:0003->0006
    0006 push b           sp:3000->2ffe pc:0006->0007 mem[2fff]:?->12 mem[2ffe]:?->34
    0007 call 0x0010      sp:2ffe->2ffc pc:0007->0010 mem[2ffd]:?->00 mem[2ffc]:?->0a
    0010 inr a            a:00->01 pc:0010->0011
    0011 ret              sp:2ffc->2ffe pc:0011->000a
    000a pop d            d:00->12 e:00->34 sp:2ffe->3000 pc:000a->000b
    000b in 0x07          a:01->5a pc:000b->000d in[07]=5a
    000d out 0x08         pc:000d->000f out[08]=5a
    000f hlt              pc:000f->0010
    --
    10 instructions
    regs a=5a b=12 c=34 d=12 e=34 h=00 l=00 f=02 sp=3000 pc=0010
    mem 0000: 31 00 30 01 34 12 c5 cd 10 00 d1 db 07 d3 08 76 3c c9
    mem 2ffc: 0a 00 34 12
    |}]

let%expect_test "without registers in the manifest only memory and ports are updated" =
  show (observe ~regs:false (Model_trace.frames sum_loop));
  [%expect
    {|
    0000 lxi h, 0x2000    regs=unobserved
    0003 mvi m, 0x55      regs=unobserved mem[2000]:?->55
    0005 mvi b, 0x03      regs=unobserved
    0007 xra a            regs=unobserved
    0008 add b            regs=unobserved
    0009 dcr b            regs=unobserved
    000a jnz 0x0008       regs=unobserved
    0008 add b            regs=unobserved
    0009 dcr b            regs=unobserved
    000a jnz 0x0008       regs=unobserved
    0008 add b            regs=unobserved
    0009 dcr b            regs=unobserved
    000a jnz 0x0008       regs=unobserved
    000d mov m, a         regs=unobserved mem[2000]:55->06
    000e hlt              regs=unobserved
    --
    15 instructions
    regs unobserved
    mem 0000: 21 00 20 36 55 06 03 af 80 05 c2 08 00 77 76
    mem 2000: 06
    |}]
