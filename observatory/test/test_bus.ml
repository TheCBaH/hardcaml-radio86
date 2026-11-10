open I8080_trace
open Helpers
open Reference

let frame ?(a = None) ?(dout = None) ?(din = None) ?(sync = Some false) ?(dbin = Some false)
    ?(wr_n = Some true) ?(wait = Some false) ?(reset = Some false) n : Frame.t =
  {
    n;
    time = Int64.of_int n;
    a;
    dout;
    din;
    sync;
    dbin;
    wr_n;
    wait;
    hlda = Some false;
    inte = Some false;
    ready = Some true;
    hold = Some false;
    int_ = Some false;
    reset;
    regs = None;
  }

let run frames = print (Pipeline.run Manifest.Fetch_accepted (List.to_seq frames))

let%expect_test "unknown data is reported, never read as zero" =
  run
    [
      frame 0 ~a:(Some 0x10) ~dout:(Some 0xa2) ~sync:(Some true);
      frame 1 ~dbin:(Some true);
      frame 2 ~din:None;
    ];
  [%expect
    {|
    note at=2 the accepted byte on the data bus is unknown
    xfer fetch       a=0010 d=~ T0-2 waits=0
    instr #0 0010: ? [] T0-2 3T regs=unobserved INCOMPLETE: the opcode byte is unknown
    end tstates=3 first_t1=0 |}]

let%expect_test "unknown pins" =
  run
    [
      frame 0 ~a:(Some 0x10) ~dout:None ~sync:(Some true);
      frame 1 ~dbin:None ~wait:None;
      frame 2 ~sync:None;
    ];
  [%expect
    {|
    note at=0 status word on the data bus is unknown
    note at=1 WAIT is unknown (X or Z)
    note at=1 DBIN is unknown (X or Z)
    note at=2 SYNC is unknown (X or Z)
    abandoned other       a=0010 d=~ T0-0 waits=0: end of the capture
    end tstates=3 first_t1=0 |}]

let%expect_test "a cycle cut by reset and one cut by the end of the capture" =
  run
    [
      frame 0 ~a:(Some 0x10) ~dout:(Some 0xa2) ~sync:(Some true);
      frame 1 ~dbin:(Some true);
      frame 2 ~reset:(Some true);
      frame 3 ~reset:(Some true);
      frame 4 ~reset:(Some false);
      frame 5 ~a:(Some 0) ~dout:(Some 0xa2) ~sync:(Some true);
      frame 6 ~dbin:(Some true);
      frame 7 ~din:(Some 0x3e);
      frame 8 ~a:(Some 1) ~dout:(Some 0x82) ~sync:(Some true);
      frame 9 ~dbin:(Some true);
    ];
  [%expect
    {|
    reset epoch=1 at=2
    abandoned fetch       a=0010 d=~ T0-0 waits=0: reset
    instr #0 0010: ? [] T0-0 1T regs=unobserved INCOMPLETE: reset
    xfer fetch       a=0000 d=3e T5-7 waits=0
    abandoned mem_read    a=0001 d=~ T8-8 waits=0: end of the capture
    instr #1 0000: ? [3e] T5-7 3T regs=unobserved INCOMPLETE: mem_read cycle at T-state 8 abandoned: end of the capture
    end tstates=10 first_t1=0 |}]

let%expect_test "an unlisted status word is kept as it is" =
  run
    [
      frame 0 ~a:(Some 0x10) ~dout:(Some 0x02) ~sync:(Some true);
      frame 1 ~dbin:(Some true);
      frame 2 ~din:(Some 0x55);
    ];
  [%expect
    {|
    xfer other       a=0010 d=55 T0-2 waits=0
    note at=2 a bus transfer with no instruction open (capture began mid-instruction)
    end tstates=3 first_t1=0 |}]

let%expect_test "stack and I/O cycles" =
  run
    [
      frame 0 ~a:(Some 0) ~dout:(Some 0xa2) ~sync:(Some true);
      frame 1 ~dbin:(Some true);
      frame 2 ~din:(Some 0xdb);
      frame 3;
      frame 4 ~a:(Some 1) ~dout:(Some 0x82) ~sync:(Some true);
      frame 5 ~dbin:(Some true);
      frame 6 ~din:(Some 0x05);
      frame 7 ~a:(Some 0x0505) ~dout:(Some 0x42) ~sync:(Some true);
      frame 8 ~dbin:(Some true);
      frame 9 ~din:(Some 0x99);
      frame 10 ~a:(Some 2) ~dout:(Some 0xa2) ~sync:(Some true);
    ];
  [%expect
    {|
    xfer fetch       a=0000 d=db T0-2 waits=0
    xfer mem_read    a=0001 d=05 T4-6 waits=0
    xfer io_read     a=0505 d=99 T7-9 waits=0
    abandoned fetch       a=0002 d=~ T10-10 waits=0: end of the capture
    instr #0 0000: in 0x05 [db 05] T0-9 10T regs=unobserved INCOMPLETE: end of the capture
    instr #1 0002: ? [] T10-10 1T regs=unobserved INCOMPLETE: fetch cycle at T-state 10 abandoned: end of the capture
    end tstates=11 first_t1=0 |}]
