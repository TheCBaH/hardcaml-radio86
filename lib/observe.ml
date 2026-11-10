open Hwfrp
module Reference = Vcd_types.Reference

type result = {
  events : Events.t list;
  frames : Frame.t list;
  tstates : int;
  enabled : int;
  diagnostics : Diag.t list;
}

let at p = Reference.of_list p

let rule_of = function
  | Manifest.After_rising -> Trace.After_rising
  | Manifest.After_falling -> Trace.After_falling
  | Manifest.Before_rising -> Trace.Before_rising

let valid_name (s : Manifest.signal) = s.name ^ ".valid"
let unbound (req, status) = Format.asprintf "%a" Bind.pp_entry (req, status)

let make_clock (m : Manifest.t) tr net =
  Bind.clock net tr ~model:"i8080" ~purpose:"T-state clock" ~name:"tstate"
    ?enable:(Option.map at m.clock.enable) (rule_of m.clock.rule) (at m.clock.path)

(* A scope of exactly the manifest's names. *)
let flat (m : Manifest.t) tr net clk =
  Bind.flat net tr clk ~model:"i8080"
    (List.concat_map
       (fun (s : Manifest.signal) ->
         (s.name, at s.path)
         :: (match s.valid with Some v -> [ (valid_name s, at v) ] | None -> []))
       m.signals)

(* With [collect] every probe that does not resolve is recorded, so the error lists all that are missing. *)
let bind_check (m : Manifest.t) tr =
  let net = Net.create () in
  Bind.collect net;
  let (Clock.Any clk) = make_clock m tr net in
  let scope = flat m tr net clk in
  List.iter
    (fun (s : Manifest.signal) ->
      ignore (Bind.resolve ~purpose:s.name scope s.name (Bind.Exact s.width));
      if s.valid <> None then
        ignore (Bind.resolve ~purpose:(valid_name s) scope (valid_name s) (Bind.Exact 1)))
    m.signals;
  match List.map unbound (Bind.failures net) with [] -> Ok () | l -> Error l

let to_int_opt l = Option.map Bits.to_int_exn (Logic.to_known l)
let to_bool_opt l = Option.map (fun v -> v <> 0) (to_int_opt l)

let run ?(frames = false) (m : Manifest.t) tr =
  match bind_check m tr with
  | Error e -> Error e
  | Ok () ->
      let net = Net.create () in
      let (Clock.Any clk) = make_clock m tr net in
      let scope = flat m tr net clk in
      let probe (s : Manifest.signal) = Bind.signal ~purpose:s.name scope s.name Bind.logic in
      let pins = List.filter (fun (s : Manifest.signal) -> s.kind = Manifest.Pin) m.signals in
      let regs = List.filter (fun (s : Manifest.signal) -> s.kind = Manifest.Register) m.signals in
      let with_valid = List.filter (fun (s : Manifest.signal) -> s.valid <> None) m.signals in
      let valid_sigs =
        if with_valid = [] then Sig.const clk []
        else
          Sig.all
            (List.map
               (fun s -> Bind.signal ~purpose:(valid_name s) scope (valid_name s) Bind.logic)
               with_valid)
      in
      let pin_sigs = Sig.all (List.map probe pins) in
      let reg_sigs = if regs = [] then Sig.const clk [] else Sig.all (List.map probe regs) in
      let pin_sigs = Sig.map2 (fun p v -> (p, v)) pin_sigs valid_sigs in
      let frame_sig =
        Sig.map2
          (fun (pl, vl) rl ->
            let tbl = Hashtbl.create 32 in
            List.iter2 (fun (s : Manifest.signal) l -> Hashtbl.replace tbl s.name l) pins pl;
            List.iter2 (fun (s : Manifest.signal) l -> Hashtbl.replace tbl s.name l) regs rl;
            List.iter2 (fun s l -> Hashtbl.replace tbl (valid_name s) l) with_valid vl;
            let driven n =
              match Hashtbl.find_opt tbl (n ^ ".valid") with
              | None -> true
              | Some l -> to_int_opt l = Some 1
            in
            let i n = if driven n then Option.bind (Hashtbl.find_opt tbl n) to_int_opt else None in
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
                    swap =
                      (if List.exists (fun (s : Manifest.signal) -> s.name = "xchg") regs then
                         Frame.Flag (i "xchg")
                       else Frame.Fixed);
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
      let sampler = Sampler.create tr (Net.domains net) in
      let stats = Sched.run sched sampler in
      Ok
        {
          events = List.rev !events;
          frames = List.rev !fr;
          tstates = !count;
          enabled = stats.Sched.enabled.(0);
          diagnostics = List.of_seq (Diag.stream net) @ Diag.summarize sampler;
        }
