open Hwfrp
module Reference = Vcd_types.Reference

type result = {
  events : Events.t list;
  frames : Frame.t list;
  tstates : int;
  diagnostics : Diag.t list;
}

let at p = Reference.of_list p

let rule_of = function
  | Manifest.After_rising -> Trace.After_rising
  | Manifest.After_falling -> Trace.After_falling
  | Manifest.Before_rising -> Trace.Before_rising

let unbound (req, status) = Format.asprintf "%a" Bind.pp_entry (req, status)

(* every probe is resolved first, so that the error lists all that are missing *)
let bind_check (m : Manifest.t) tr =
  let net = Net.create () in
  let errs = ref [] in
  let add e = errs := e :: !errs in
  let try_ f = try f () with Bind.Unbound (req, st) -> add (unbound (req, st)) in
  (match
     try
       Some
         (Bind.clock net tr ~model:"i8080" ~purpose:"T-state clock" ~name:"tstate"
            ?enable:(Option.map at m.clock.enable) (rule_of m.clock.rule) (at m.clock.path))
     with Bind.Unbound (req, st) ->
       add (unbound (req, st));
       None
   with
  | None -> ()
  | Some (Clock.Any clk) ->
      let scope = Bind.root net tr clk ~model:"i8080" (at []) in
      List.iter
        (fun (s : Manifest.signal) ->
          try_ (fun () ->
              ignore
                (Bind.resolve ~purpose:s.name
                   (Bind.root net tr clk ~model:"i8080" ~overrides:[ (s.name, at s.path) ] (at []))
                   s.name (Bind.Exact s.width))))
        m.signals;
      ignore scope);
  match List.rev !errs with [] -> Ok () | l -> Error l

let to_int_opt l = Option.map Bits.to_int_exn (Logic.to_known l)
let to_bool_opt l = Option.map (fun v -> v <> 0) (to_int_opt l)

let run ?(frames = false) (m : Manifest.t) tr =
  match bind_check m tr with
  | Error e -> Error e
  | Ok () ->
      let net = Net.create () in
      let (Clock.Any clk) =
        Bind.clock net tr ~model:"i8080" ~purpose:"T-state clock" ~name:"tstate"
          ?enable:(Option.map at m.clock.enable) (rule_of m.clock.rule) (at m.clock.path)
      in
      let overrides = List.map (fun (s : Manifest.signal) -> (s.name, at s.path)) m.signals in
      let scope = Bind.root net tr clk ~model:"i8080" ~overrides (at []) in
      let probe (s : Manifest.signal) = Bind.signal ~purpose:s.name scope s.name Bind.logic in
      let pins = List.filter (fun (s : Manifest.signal) -> s.kind = Manifest.Pin) m.signals in
      let regs = List.filter (fun (s : Manifest.signal) -> s.kind = Manifest.Register) m.signals in
      let pin_sigs = Sig.all (List.map probe pins) in
      let reg_sigs = if regs = [] then Sig.const clk [] else Sig.all (List.map probe regs) in
      let frame_sig =
        Sig.map2
          (fun pl rl ->
            let tbl = Hashtbl.create 32 in
            List.iter2 (fun (s : Manifest.signal) l -> Hashtbl.replace tbl s.name l) pins pl;
            List.iter2 (fun (s : Manifest.signal) l -> Hashtbl.replace tbl s.name l) regs rl;
            let i n = Option.bind (Hashtbl.find_opt tbl n) to_int_opt in
            let b n = Option.bind (Hashtbl.find_opt tbl n) to_bool_opt in
            let regs_v =
              if regs = [] then None
              else
                Some
                  {
                    Frame.acc = i "acc";
                    bc = i "bc";
                    de = i "de";
                    hl = i "hl";
                    sp = i "sp";
                    pc = i "pc";
                    wz = i "wz";
                    z = i "z";
                    s = i "s";
                    p = i "p";
                    c = i "c";
                    ac = i "ac";
                  }
            in
            ( i "a",
              i "do",
              i "di",
              b "sync",
              b "dbin",
              b "wr_n",
              b "wait",
              b "hlda",
              b "inte",
              b "ready",
              b "hold",
              b "int",
              b "reset",
              regs_v ))
          pin_sigs reg_sigs
      in
      let raw =
        Ev.when_ (Sig.enabled clk) (Sig.map2 (fun v t -> (v, t)) frame_sig (Sig.time clk))
      in
      let frames_ev =
        Ev.scan ~init:0
          (fun n
               ( (a, dout, din, sync, dbin, wr_n, wait, hlda, inte, ready, hold, int_, reset, regs),
                 time ) ->
            ( n + 1,
              Some
                {
                  Frame.n;
                  time = Vcd_types.Timestamp.to_int64 time;
                  a;
                  dout;
                  din;
                  sync;
                  dbin;
                  wr_n;
                  wait;
                  hlda;
                  inte;
                  ready;
                  hold;
                  int_;
                  reset;
                  regs;
                } ))
          raw
      in
      let spec =
        {
          Machine.name = "i8080_observe";
          init = Pipeline.init m.snapshot;
          step =
            (fun ~tick:_ st -> function
              | None -> (st, [])
              | Some f ->
                  let st, evs = Pipeline.step st f in
                  (st, evs));
          finish = (fun st -> Pipeline.finish st);
          every_tick = true;
        }
      in
      let events_ev, _ = Machine.run spec (Ev.current frames_ev) in
      let events = ref [] and fr = ref [] and count = ref 0 in
      Ev.subscribe events_ev (fun evs -> List.iter (fun e -> events := e :: !events) evs);
      Ev.subscribe frames_ev (fun f ->
          incr count;
          if frames then fr := f :: !fr);
      let sched =
        match Sched.compile net with Ok s -> s | Error _ -> failwith "i8080_observe: net"
      in
      ignore (Sched.run sched (Sampler.create tr (Net.domains net)));
      Ok
        {
          events = List.rev !events;
          frames = List.rev !fr;
          tstates = !count;
          diagnostics = List.of_seq (Diag.stream net);
        }
