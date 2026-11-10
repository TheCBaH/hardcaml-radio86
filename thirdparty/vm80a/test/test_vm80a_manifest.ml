(* The manifest of the released vm80a_sim: valid, and the claims the rest of the work rests on. *)
open I8080_trace

let load () = match Manifest.load "../manifest.json" with Ok m -> m | Error e -> failwith e

let%expect_test "the vm80a manifest" =
  let m = load () in
  (match Manifest.validate m with
  | Ok () -> print_endline "valid"
  | Error l -> List.iter print_endline l);
  Printf.printf "%s, %d signals, registers: %b, snapshot at %s\n" m.id (List.length m.signals)
    (Manifest.has_registers m)
    (match m.snapshot with
    | Manifest.Fetch_accepted -> "the accepted fetch"
    | Manifest.Fetch_t1 -> "the fetch T1");
  [%expect
    {|
    valid
    vm80a-fst, 26 signals, registers: true, snapshot at the accepted fetch |}]

let%expect_test "the JSON round trip is stable" =
  let m = load () in
  (match Manifest.of_string (Manifest.to_string m) with
  | Ok m' -> Printf.printf "%b\n" (m = m' && Manifest.to_string m' = Manifest.to_string m)
  | Error e -> print_endline e);
  [%expect {| true |}]
