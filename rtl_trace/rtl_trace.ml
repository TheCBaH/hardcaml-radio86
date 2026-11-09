module Frame = I8080_trace.Frame

(* The pins of a Cyclesim run as the frames the observation library compares; samples before the first
   SYNC are the implementation latency and are left out, like the K4 pin trace does. *)
let frames (r : I8080_rtl.Harness.result) =
  let b x = Some x and i x = Some x in
  List.filter_map
    (fun (s : I8080_rtl.Harness.sample) ->
      if s.n < r.pre_sync then None
      else
        Some
          {
            Frame.n = s.n - r.pre_sync;
            time = Int64.of_int s.n;
            a = i s.a;
            dout = s.dout;
            din = s.din;
            sync = b s.sync;
            dbin = b s.dbin;
            wr_n = b s.wr_n;
            wait = b s.wait;
            hlda = b s.hlda;
            inte = b s.inte;
            ready = b s.ready;
            hold = b s.hold;
            int_ = b s.int_;
            reset = b s.reset;
            regs = None;
          })
    r.samples
