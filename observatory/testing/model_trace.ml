(* The pin frames of a program, made by running it in Rivet's behavioural model and giving every
   instruction's bus cycles the K4 shape, with the registers a CPU would show: those after the previous
   instruction (the write-back lag of the Fetch_accepted snapshot), PC one past the fetch address. The
   result goes through Synth and hwfrp, so the observer is tested on programs the RTL cannot run yet. *)
open I8080_trace
module C = I8080_sem.Concrete_of (Compute.Concrete)

type cycle = { status : int; addr : int; data : int option; write : bool }

let regs_of (r : I8080_sem.regs) ~pc : Frame.regs =
  let bit n = Some ((r.flags lsr n) land 1) in
  {
    Frame.acc = Some r.ra;
    bc = Some ((r.rb lsl 8) lor r.rc);
    de = Some ((r.rd lsl 8) lor r.re);
    hl = Some ((r.rh lsl 8) lor r.rl);
    sp = Some r.rsp;
    pc = Some pc;
    wz = Some 0;
    z = bit 6;
    s = bit 7;
    p = bit 2;
    c = bit 0;
    ac = bit 4;
    swap = Frame.Fixed;
  }

let pins ?(a = None) ?(dout = None) ?(din = None) ?(sync = false) ?(dbin = false) ?(wr_n = true)
    ?(wait = false) regs : Frame.t =
  {
    Frame.n = 0;
    time = 0L;
    a;
    dout;
    din;
    sync = Some sync;
    dbin = Some dbin;
    wr_n = Some wr_n;
    wait = Some wait;
    hlda = Some false;
    inte = Some false;
    ready = Some true;
    hold = Some false;
    int_ = Some false;
    reset = Some false;
    regs = Some regs;
  }

let cycle_frames regs ~extra { status; addr; data; write } =
  let t1 = pins ~a:(Some addr) ~dout:(Some status) ~sync:true regs in
  let d = data in
  let body =
    if write then [ pins ~a:(Some addr) ~dout:d regs; pins ~dout:d ~wr_n:false regs ]
    else [ pins ~a:(Some addr) ~din:d ~dbin:true regs; pins ~din:d regs ]
  in
  (t1 :: body) @ List.init extra (fun _ -> pins regs)

let number frames = List.mapi (fun n (f : Frame.t) -> { f with Frame.n }) frames

(* [program] is loaded at [origin]; the run ends at HLT or after [limit] instructions. *)
let frames ?(origin = 0) ?(sp = 0x3000) ?(limit = 100) ?(input = 0x5a) program =
  let st =
    C.create ~env:{ C.input = (fun _ -> input); output = (fun _ _ -> ()) } (C.create_memory ())
  in
  C.poke st origin program;
  st.C.pc <- origin;
  st.C.sp <- sp;
  let log = ref [] in
  st.C.log <- Some (fun e -> log := e :: !log);
  let out = ref [] in
  let add l = out := List.rev_append l !out in
  let rec go n =
    if n >= limit then ()
    else begin
      let before = C.regs st in
      log := [];
      let t0 = st.C.tstates in
      C.step st;
      let after = C.regs st in
      let tstates = st.C.tstates - t0 in
      let regs = regs_of before ~pc:((before.I8080_sem.rpc + 1) land 0xffff) in
      let cycles =
        List.mapi
          (fun k e ->
            match e with
            | C.Fetch (a, v) ->
                { status = (if k = 0 then 0xa2 else 0x82); addr = a; data = Some v; write = false }
            | C.Read (a, v) -> { status = 0x82; addr = a; data = Some v; write = false }
            | C.Write (a, v) -> { status = 0x00; addr = a; data = Some v; write = true }
            | C.In (p, v) -> { status = 0x42; addr = p lor (p lsl 8); data = Some v; write = false }
            | C.Out (p, v) -> { status = 0x10; addr = p lor (p lsl 8); data = Some v; write = true })
          (List.rev !log)
      in
      if st.C.halted then begin
        add (cycle_frames regs ~extra:1 (List.hd cycles));
        let r = regs_of after ~pc:after.I8080_sem.rpc in
        add
          [
            pins ~dout:(Some 0x8a) ~sync:true r;
            pins r;
            pins ~wait:true r;
            pins ~wait:true r;
            pins ~wait:true r;
          ]
      end
      else begin
        let rest = List.length cycles - 1 in
        let m1 = tstates - (3 * rest) in
        List.iteri
          (fun k c -> add (cycle_frames regs ~extra:(if k = 0 then m1 - 3 else 0) c))
          cycles;
        go (n + 1)
      end
    end
  in
  go 0;
  number (List.rev !out)
