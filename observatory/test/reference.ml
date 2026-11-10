(* The manifest of the synthetic bench that Synth writes; every pin and register sits directly under
   [i8080_tb], so it is not any RTL's manifest. *)
open I8080_trace

let manifest () = match Manifest.load "reference.json" with Ok m -> m | Error e -> failwith e

(* With the registers of Model_trace, which has no exchange state. *)
let with_regs () =
  let m = manifest () in
  { m with signals = List.filter (fun (s : Manifest.signal) -> s.name <> "xchg") m.signals }
