open I8080_trace
open Helpers
open Reference

let%expect_test "the no-wait hand trace" =
  print (Pipeline.run (manifest ()).snapshot (List.to_seq (frames_of Traces.nowait)));
  [%expect
    {|
    xfer fetch       a=0000 d=3e T0-2 waits=0
    xfer mem_read    a=0001 d=2a T4-6 waits=0
    xfer fetch       a=0002 d=32 T7-9 waits=0
    instr #0 0000: mvi a, 0x2a [3e 2a] T0-6 7T regs=unobserved
    xfer mem_read    a=0003 d=00 T11-13 waits=0
    xfer mem_read    a=0004 d=20 T14-16 waits=0
    xfer mem_write   a=2000 d=2a T17-19 waits=0
    xfer fetch       a=0005 d=76 T20-22 waits=0
    instr #1 0002: sta 0x2000 [32 00 20] T7-19 13T regs=unobserved
    xfer halt_ack    a=~ d=~ T24-26 waits=0
    instr #2 0005: hlt [76] T20-26 7T regs=unobserved
    halt-evidence first=24 n=18:a=2000,pc=~ n=19:a=~,pc=~ n=20:a=0005,pc=~ n=21:a=0005,pc=~ n=22:a=0005,pc=~ n=23:a=~,pc=~ n=24:a=~,pc=~ n=25:a=~,pc=~ n=26:a=~,pc=~ n=27:a=~,pc=~ n=28:a=~,pc=~
    end tstates=29 first_t1=0 |}]

let%expect_test "two waits on the STA write" =
  print (Pipeline.run (manifest ()).snapshot (List.to_seq (frames_of Traces.wait2)));
  [%expect
    {|
    xfer fetch       a=0000 d=3e T0-2 waits=0
    xfer mem_read    a=0001 d=2a T4-6 waits=0
    xfer fetch       a=0002 d=32 T7-9 waits=0
    instr #0 0000: mvi a, 0x2a [3e 2a] T0-6 7T regs=unobserved
    xfer mem_read    a=0003 d=00 T11-13 waits=0
    xfer mem_read    a=0004 d=20 T14-16 waits=0
    xfer mem_write   a=2000 d=2a T17-21 waits=2
    xfer fetch       a=0005 d=76 T22-24 waits=0
    instr #1 0002: sta 0x2000 [32 00 20] T7-21 15T regs=unobserved
    xfer halt_ack    a=~ d=~ T26-28 waits=0
    instr #2 0005: hlt [76] T22-28 7T regs=unobserved
    halt-evidence first=26 n=20:a=2000,pc=~ n=21:a=~,pc=~ n=22:a=0005,pc=~ n=23:a=0005,pc=~ n=24:a=0005,pc=~ n=25:a=~,pc=~ n=26:a=~,pc=~ n=27:a=~,pc=~ n=28:a=~,pc=~ n=29:a=~,pc=~ n=30:a=~,pc=~
    end tstates=31 first_t1=0 |}]
