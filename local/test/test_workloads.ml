(* The math workloads (workloads/) on the Hardcaml core: each fixture's image is loaded at its origin,
   the core runs from reset to HLT with no shim and no boot preamble, and the result is checked against
   the independent expectation, the declared writable regions, the guards and the T-state budget. Then
   the same run is replayed instruction by instruction in Rivet's model (lockstep), and its final
   memory and T-state count are compared with the model's own run. *)
open I8080_trace
open I8080_rtl
open I8080_workloads

let manifest () = match Manifest.load "../manifest.json" with Ok m -> m | Error e -> failwith e
let stim () = match Stimulus.parse "reset 0 @4\n" with Ok s -> s | Error e -> failwith e

let port_traffic (r : Harness.result) =
  r.io_out <> []
  || List.exists
       (fun (s : Harness.sample) -> s.sync && (s.dout = Some 0x42 || s.dout = Some 0x10))
       r.samples

let run_core ?vcd (t : Fixture.t) =
  let memory = Harness.new_memory () in
  List.iter (fun (origin, bytes) -> Harness.load_program memory ~origin bytes) (Fixture.image t);
  let r = Harness.run ?vcd ~max_tstates:(t.budget + 64) ~stim:(stim ()) ~memory () in
  ( r,
    {
      Fixture.halted = r.stop = Harness.Halt;
      trapped = r.trapped <> None;
      tstates = Option.value r.counted ~default:max_int;
      writes = List.map (fun (w : Harness.write) -> (w.addr, w.data)) r.writes;
      port_traffic = port_traffic r;
      memory = r.memory;
    } )

let%expect_test "the workloads halt on the core with the expected RAM, and equal the model" =
  List.iter
    (fun (t : Fixture.t) ->
      let _, o = run_core t in
      let m = Model_run.run t in
      let errs = Fixture.check t o in
      Printf.printf "%s: core %d T-states, model %d, memory %s, writes %s %s\n" t.name o.tstates
        m.tstates
        (if Bytes.equal o.memory m.memory then "equal" else "DIFFERENT")
        (if o.writes = m.writes then "equal" else "DIFFERENT")
        (if errs = [] then "ok" else "FAILED");
      List.iter (Printf.printf "  %s\n") errs)
    (Fixtures.all ());
  [%expect
    {|
    ackermann: core 41782 T-states, model 41782, memory equal, writes equal ok
    multiply: core 12919 T-states, model 12919, memory equal, writes equal ok
    divide: core 34356 T-states, model 34356, memory equal, writes equal ok
    |}]

let%expect_test "the workloads in lockstep with Rivet's model" =
  List.iter
    (fun (t : Fixture.t) ->
      let file = Filename.temp_file "workload" ".vcd" in
      let oc = open_out_bin file in
      let r, _ = run_core ~vcd:oc t in
      close_out oc;
      let m = manifest () in
      let res =
        match Observe.run ~frames:true m (Hwfrp_native.open_file file) with
        | Ok o -> o
        | Error l -> failwith (String.concat "\n" l)
      in
      Sys.remove file;
      let times = Hashtbl.create 64 in
      List.iter (fun (f : Frame.t) -> Hashtbl.replace times f.n f.time) res.frames;
      let setup =
        { Lockstep.memory = Fixture.image t; pc = 0; sp = 0; known = []; start_regs = None }
      in
      let rep =
        Lockstep.run setup ~time_of:(Hashtbl.find_opt times) (Events.instructions res.events)
      in
      Printf.printf "%s: %d instructions compared, halted=%b, %s\n" t.name rep.compared rep.halted
        (match rep.divergence with
        | None -> "no divergence"
        | Some d -> Format.asprintf "%a" Lockstep.pp_divergence d);
      ignore r)
    (Fixtures.all ());
  [%expect
    {|
    ackermann: 5501 instructions compared, halted=true, no divergence
    multiply: 2023 instructions compared, halted=true, no divergence
    divide: 4430 instructions compared, halted=true, no divergence
    |}]
