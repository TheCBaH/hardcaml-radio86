open I8080_workloads

let run_model t = Model_run.run t

let report (t : Fixture.t) =
  let o = run_model t in
  let errs = Fixture.check t o in
  Printf.printf "%s: code=%d data=%d tstates=%d stack=%d budget=%d %s\n" t.name
    (String.length t.code.I8080_asm.Program.bytes)
    (String.length t.data.I8080_asm.Program.bytes)
    o.tstates (Fixture.stack_depth o) t.budget
    (if errs = [] then "ok" else "FAILED");
  List.iter (Printf.printf "  %s\n") errs

(* {1 The oracles} *)

let%expect_test "ackermann: the definition, the closed forms and the proposal's table" =
  for m = 0 to 3 do
    for n = 0 to 8 do
      assert (Oracle.ackermann m n = Oracle.ackermann_closed m n)
    done
  done;
  List.iter
    (fun (m, n, want) ->
      Printf.printf "A(%d,%d)=%d %b\n" m n (Oracle.ackermann m n) (Oracle.ackermann m n = want))
    [ (0, 0, 1); (0, 7, 8); (1, 4, 6); (2, 4, 11); (3, 2, 29); (4, 0, 13); (4, 1, 65533) ];
  [%expect
    {|
    A(0,0)=1 true
    A(0,7)=8 true
    A(1,4)=6 true
    A(2,4)=11 true
    A(3,2)=29 true
    A(4,0)=13 true
    A(4,1)=65533 true |}]

let%expect_test "multiply and divide oracles" =
  Printf.printf "%04x %04x %04x\n" (Oracle.mul16 0xffff 0xffff) (Oracle.mul16 0x8000 2)
    (Oracle.mul16 0x1234 0x5678);
  let q, r = Oracle.div32x16 ~hi:2 ~lo:0xffff ~d:3 in
  Printf.printf "%04x %04x %b\n" q r (Oracle.div_consistent ~hi:2 ~lo:0xffff ~d:3 ~q ~r);
  Printf.printf "%b\n" (Oracle.div_consistent ~hi:2 ~lo:0xffff ~d:3 ~q:(q - 1) ~r);
  (match Oracle.div32x16 ~hi:3 ~lo:0 ~d:3 with
  | _ -> print_endline "accepted"
  | exception Invalid_argument _ -> print_endline "quotient would not fit: refused");
  [%expect
    {|
    0001 0000 0060
    ffff 0002 true
    false
    quotient would not fit: refused
    |}]

let%expect_test "pi digits: Machin's formula against the digits written down" =
  Printf.printf "%b\n" (Oracle.pi_digits 32 = Oracle.pi_32);
  let fifty = "31415926535897932384626433832795028841971693993751" in
  Printf.printf "%b\n" (Oracle.pi_digits 50 = fifty);
  let long = Oracle.pi_digits 1000 in
  Printf.printf "%b %d\n" (String.sub long 0 50 = fifty) (String.length long);
  [%expect {|
    true
    true
    true 1000 |}]

(* {1 The workloads in Rivet's model} *)

let%expect_test "the three fixtures" =
  List.iter report (Fixtures.all ());
  [%expect
    {|
    ackermann: code=86 data=66 tstates=41782 stack=110 budget=50000 ok
    multiply: code=90 data=113 tstates=12919 stack=2 budget=16000 ok
    divide: code=135 data=183 tstates=34356 stack=12 budget=42000 ok
    |}]

let%expect_test "the proposal's Ackermann table" =
  let t : Fixture.t = Fixtures.ackermann [ (0, 0); (0, 7); (1, 4); (2, 4); (3, 2) ] in
  let o = run_model t in
  let at = Fixture.symbol_exn t.Fixture.data in
  let w k =
    Char.code (Bytes.get o.memory (at "results" + k))
    lor (Char.code (Bytes.get o.memory (at "results" + k + 1)) lsl 8)
  in
  Printf.printf "%d %d %d %d %d\n" (w 0) (w 2) (w 4) (w 6) (w 8);
  Printf.printf "%s\n" (String.concat ";" (Fixture.check t o));
  [%expect {| 1 8 6 11 29 |}]

(* {1 The checks catch what they are for} *)

let%expect_test "a wrong expectation, a stray write and a missed HLT are reported" =
  let t : Fixture.t = Fixtures.multiply Fixtures.multiply_cases in
  let o = run_model t in
  let wrong =
    {
      t with
      Fixture.expect =
        List.map
          (fun (a, b) ->
            (a, String.mapi (fun i c -> if i = 0 then Char.chr (Char.code c lxor 1) else c) b))
          t.Fixture.expect;
    }
  in
  List.iter print_endline (Fixture.check wrong o);
  let narrow =
    {
      t with
      Fixture.writable =
        List.filter (fun (r : Fixture.region) -> r.name <> "results") t.Fixture.writable;
    }
  in
  print_endline (List.hd (Fixture.check narrow o));
  let slow = { t with Fixture.budget = 100 } in
  print_endline (List.hd (Fixture.check slow o));
  let clobbered = Bytes.copy o.memory in
  Bytes.set clobbered (Fixture.data_origin + 3) '\000';
  print_endline (List.hd (Fixture.check t { o with memory = clobbered }));
  [%expect
    {|
    RAM at 4044: expected 01 00 00 00 01 00 ff ff ff ff 01 00 ff ff 72 1c fe ff 00 00 00 00 01 00 60 00 01 fe 00 00, got 00 00 00 00 01 00 ff ff ff ff 01 00 ff ff 72 1c fe ff 00 00 00 00 01 00 60 00 01 fe 00 00
    RAM at 4067: expected 01 f0, got 00 f0
    write of 00 to 4044 outside the writable regions
    12919 T-states exceed the budget of 100
    the data low guard at 4000 was overwritten
    |}]

(* {1 Random operands} *)

let word = QCheck.Gen.(map (fun x -> x land 0xffff) (int_bound 0xffff))

let%expect_test "random products and quotients agree with the oracle" =
  let rand = Random.State.make [| 2026; 11; 11 |] in
  let mul_cases = QCheck.Gen.(list_size (return 40) (pair word word)) in
  let div_cases =
    QCheck.Gen.(
      list_size (return 40)
        (map3
           (fun d hi lo ->
             let d = max 1 d in
             (hi mod d, lo, d))
           word word word))
  in
  let bad = ref 0 in
  for _ = 1 to 10 do
    let m = Fixtures.multiply ~budget:1_000_000 (QCheck.Gen.generate1 ~rand mul_cases) in
    if Fixture.check m (run_model m) <> [] then incr bad;
    let d = Fixtures.divide ~budget:1_000_000 (QCheck.Gen.generate1 ~rand div_cases) in
    if Fixture.check d (run_model d) <> [] then incr bad
  done;
  Printf.printf "failures=%d\n" !bad;
  [%expect {| failures=0 |}]
