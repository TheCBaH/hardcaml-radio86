(* Runs a fixture in Rivet's behavioural model (the concrete interpretation of the i8080 table).
   Authored in this repository (MIT). *)

module S = Rivet_i8080_sem.Runnable (Rivet_compute.Concrete)

let run (t : Fixture.t) : Fixture.outcome =
  let ports = ref false in
  let st =
    S.create
      ~input:(fun _ ->
        ports := true;
        0xff)
      ~output:(fun _ _ -> ports := true)
  in
  List.iter (fun (addr, bytes) -> S.poke st addr bytes) (Fixture.image t);
  let writes = ref [] in
  S.trace st
    (Some
       (function
       | Rivet_i8080_sem.Ev_write (a, v) -> writes := (a, v) :: !writes
       | Rivet_i8080_sem.Ev_in _ | Rivet_i8080_sem.Ev_out _ -> ports := true
       | _ -> ()));
  S.start st ~pc:t.code.Rivet_i8080_asm.Program.origin ~sp:0;
  while (not (S.halted st)) && S.tstates st <= t.budget do
    S.step st
  done;
  {
    Fixture.halted = S.halted st;
    trapped = false;
    tstates = S.tstates st;
    writes = List.rev !writes;
    port_traffic = !ports;
    memory = Bytes.copy (S.memory st);
  }
