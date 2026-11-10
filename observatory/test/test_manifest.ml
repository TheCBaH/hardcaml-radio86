open I8080_trace
open Helpers
open Reference

let show = function Ok () -> print_endline "valid" | Error l -> List.iter print_endline l

let%expect_test "the reference manifest" =
  let m = manifest () in
  show (Manifest.validate m);
  Printf.printf "%d signals, registers: %b, snapshot at %s\n" (List.length m.signals)
    (Manifest.has_registers m)
    (match m.snapshot with
    | Manifest.Fetch_accepted -> "the accepted fetch"
    | Manifest.Fetch_t1 -> "the fetch T1");
  [%expect {|
    valid
    26 signals, registers: true, snapshot at the accepted fetch |}]

let%expect_test "the JSON round trip is stable" =
  let m = manifest () in
  (match Manifest.of_string (Manifest.to_string m) with
  | Ok m' -> Printf.printf "%b\n" (m = m' && Manifest.to_string m' = Manifest.to_string m)
  | Error e -> print_endline e);
  [%expect {| true |}]

let%expect_test "invalid manifests" =
  let m = manifest () in
  let drop n =
    { m with signals = List.filter (fun (s : Manifest.signal) -> s.name <> n) m.signals }
  in
  show (Manifest.validate (drop "ready"));
  show (Manifest.validate (drop "bc"));
  show (Manifest.validate { m with profile = "i8085/1" });
  show
    (Manifest.validate
       {
         m with
         signals =
           { name = "a"; path = [ "x" ]; width = 16; kind = Manifest.Pin; valid = None }
           :: m.signals;
       });
  show
    (Manifest.validate
       {
         m with
         signals =
           { name = "q"; path = [ "x" ]; width = 1; kind = Manifest.Register; valid = None }
           :: m.signals;
       });
  show (Manifest.validate (pins_only m));
  [%expect
    {|
    pin ready is not declared
    register bc is not declared (registers are all or none)
    profile "i8085/1" is not "i8080a/1"
    signal a is declared twice
    q is not a register of the profile
    valid |}]

let%expect_test "unknown members and a wrong format are rejected" =
  (match Manifest.of_string {|{"format":"i8080-observation-manifest/2"}|} with
  | Ok _ -> print_endline "ok"
  | Error e -> print_endline e);
  (match
     Manifest.of_string
       (String.concat "" [ {|{"format":"i8080-observation-manifest/1","extra":1}|} ])
   with
  | Ok _ -> print_endline "ok"
  | Error e -> print_endline e);
  [%expect
    {|
    Unexpected format enum string value: i8080-observation-manifest/2.
    Should it be i8080-observation-manifest/1 ?
    File "-":
    File "-": in member format of
    File "-", line 1, characters 0-41: observation manifest object
    Missing members in observation manifest object:
     clock
     id
     profile
     signals
     snapshot
    File "-", line 1, characters 0-51: |}]

let%expect_test "SHA-256 vectors" =
  print_endline (Sha256.hex "");
  print_endline (Sha256.hex "abc");
  print_endline (Sha256.hex (String.make 1000 'a'));
  [%expect
    {|
    e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
    41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3 |}]
