open Hwfrp
module Reference = Vcd_types.Reference

let () =
  let file = Sys.argv.(1) and rule = Sys.argv.(2) in
  let tr = Hwfrp_native.open_file file in
  let net = Net.create () in
  let at p = Reference.of_list p in
  let rule =
    match rule with
    | "r" -> Trace.After_rising
    | "f" -> Trace.After_falling
    | _ -> Trace.Before_rising
  in
  let (Clock.Any clk) =
    Bind.clock net tr ~model:"probe" ~name:"clk"
      ~enable:(at [ "i8080_tb"; "f2" ])
      rule
      (at [ "i8080_tb"; "clk" ])
  in
  let scope = Bind.root net tr clk ~model:"probe" (at [ "i8080_tb" ]) in
  let a = Bind.signal scope "pin_a" Bind.uint in
  let d = Bind.signal scope "pin_dout" Bind.uint in
  let di = Bind.signal scope "pin_din" Bind.uint in
  let sync = Bind.signal scope "pin_sync" Bind.uint in
  let ev =
    Ev.when_ (Sig.enabled clk)
      (Sig.map2
         (fun (a, d) (di, s) -> (a, d, di, s))
         (Sig.map2 (fun a d -> (a, d)) a d)
         (Sig.map2 (fun a d -> (a, d)) di sync))
  in
  let ev = Ev.tag (Sig.time clk) (Ev.tag (Sig.cycle clk) ev) in
  Ev.subscribe ev (fun (((a, d, di, s), c), t) ->
      Printf.printf "c=%d t=%Ld a=%04x do=%02x di=%02x sync=%d\n" c
        (Int64.of_int (Obj.magic t))
        a d di s);
  let sched = Result.get_ok (Sched.compile net) in
  ignore (Sched.run sched (Sampler.create tr (Net.domains net)))
