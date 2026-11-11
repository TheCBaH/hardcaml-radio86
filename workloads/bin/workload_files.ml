(* Writes each fixture as files for an external CPU: NAME.img (the initial RAM as hex bytes from 0 to the
   end of the stack guard, the format of vm80a_sim's +image), NAME.final (the 64 KiB that Rivet's model
   leaves), NAME.tstates and NAME.budget. Authored in this repository (MIT).
   usage: workload_files OUTDIR *)
open I8080_workloads

let () =
  let dir =
    match Sys.argv with
    | [| _; d |] -> d
    | _ ->
        prerr_endline "usage: workload_files OUTDIR";
        exit 2
  in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  List.iter
    (fun (t : Fixture.t) ->
      let memory = Bytes.make 0x10000 '\000' in
      List.iter (fun (a, b) -> Bytes.blit_string b 0 memory a (String.length b)) (Fixture.image t);
      let top = fst Fixture.stack_guard + Fixture.guard_size in
      let write name f =
        let oc = open_out_bin (Filename.concat dir (t.name ^ "." ^ name)) in
        f oc;
        close_out oc
      in
      write "img" (fun oc ->
          Bytes.iteri (fun i c -> if i < top then Printf.fprintf oc "%02x\n" (Char.code c)) memory);
      let o = Model_run.run t in
      let problems = Fixture.check t o in
      if problems <> [] then (
        List.iter prerr_endline problems;
        exit 1);
      write "final" (fun oc -> output_bytes oc o.memory);
      write "tstates" (fun oc -> Printf.fprintf oc "%d\n" o.tstates);
      write "budget" (fun oc -> Printf.fprintf oc "%d\n" t.budget))
    (Fixtures.all ())
