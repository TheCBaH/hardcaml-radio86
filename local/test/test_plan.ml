(* The cycle plan of every opcode against Rivet's descriptor table: its length in bytes, its T-states
   with the condition false and true, and the layout of the cycles (one fetch first, one halt cycle
   after HLT only). *)
open I8080_rtl

let%expect_test "length and T-states of all 256 opcodes equal Rivet's table" =
  let bad = ref [] in
  for op = 0 to 255 do
    let t = I8080_ops.timings.(op) in
    let untaken, taken = Plan.tstates op in
    if Plan.length op <> t.length || untaken <> t.tstates || taken <> t.taken then
      bad :=
        Printf.sprintf "%02x: plan %d bytes %d/%d T, Rivet %d bytes %d/%d T" op (Plan.length op)
          untaken taken t.length t.tstates t.taken
        :: !bad
  done;
  List.iter print_endline (List.rev !bad);
  Printf.printf "%d differences\n" (List.length !bad);
  [%expect {| 0 differences |}]

let%expect_test "every plan starts with a fetch; only HLT has a halt cycle; at most five cycles" =
  let bad = ref 0 in
  for op = 0 to 255 do
    let cs = Plan.cycles op in
    let fetch_first = match cs with c :: _ -> c.Plan.status = 0xa2 | [] -> false in
    let halts = List.length (List.filter (fun c -> c.Plan.status = 0x8a) cs) in
    if
      (not fetch_first) || (halts <> if op = 0x76 then 1 else 0) || List.length cs > Plan.max_cycles
    then incr bad
  done;
  Printf.printf "%d malformed\n" !bad;
  [%expect {| 0 malformed |}]
