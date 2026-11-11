(* [Alu] evaluated on [Bits] for every input, against Rivet's behavioural model executing the
   instruction: no simulation, the same functions the core instantiates on signals. *)
open Hardcaml
module A = I8080_rtl.Alu.Make (Bits)
module M = I8080_sem.Model

let model =
  M.create ~env:{ M.input = (fun _ -> 0xff); output = (fun _ _ -> ()) } (M.create_memory ())

let b8 = Bits.of_int ~width:8
let bit x = Bits.of_int ~width:1 x

(* One instruction in the model from the given A, B and flags: A and the flag byte after it. *)
let exec ~opcode ~a ~b ~ac ~c =
  let st = model in
  st.a <- a;
  st.b <- b;
  st.s <- 0;
  st.z <- 0;
  st.p <- 0;
  st.ac <- ac;
  st.cy <- c;
  st.pc <- 0;
  M.poke st 0 (String.make 1 (Char.chr opcode));
  M.step st;
  let r = M.regs st in
  (r.ra, r.flags)

let bad = ref 0
and cases = ref 0

let check name ~inputs (res, flags) (ra, rf) =
  incr cases;
  if Bits.to_int res <> ra || Bits.to_int (A.pack flags) <> rf then begin
    incr bad;
    if !bad <= 10 then
      Printf.printf "%s %s: core %02x f=%02x, model %02x f=%02x\n" name inputs (Bits.to_int res)
        (Bits.to_int (A.pack flags))
        ra rf
  end

let report name =
  Printf.printf "%s: %d cases, %d differences\n" name !cases !bad;
  cases := 0;
  bad := 0

let%expect_test "ADD ADC SUB SBB ANA XRA ORA CMP: every A, operand and carry" =
  for op = 0 to 7 do
    for a = 0 to 255 do
      for y = 0 to 255 do
        for c = 0 to 1 do
          let res, f = A.alu ~op:(Bits.of_int ~width:3 op) ~a:(b8 a) ~y:(b8 y) ~c:(bit c) in
          (* CMP leaves A alone *)
          let res = if op = 7 then b8 a else res in
          check "alu"
            ~inputs:(Printf.sprintf "op=%d a=%02x y=%02x c=%d" op a y c)
            (res, f)
            (exec ~opcode:(0x80 lor (op lsl 3)) ~a ~b:y ~ac:0 ~c)
        done
      done
    done
  done;
  report "alu";
  [%expect {| alu: 1048576 cases, 0 differences |}]

let%expect_test "INR DCR, the rotates and DAA: every value and flag" =
  let flags_of ~ac ~c = { A.s = Bits.gnd; z = Bits.gnd; p = Bits.gnd; ac = bit ac; c = bit c } in
  for x = 0 to 255 do
    for c = 0 to 1 do
      for ac = 0 to 1 do
        let inputs = Printf.sprintf "x=%02x ac=%d c=%d" x ac c in
        List.iter
          (fun (dec, opcode) ->
            (* INR B, DCR B: the result is B; A is not part of the comparison *)
            let res, f = A.incdec ~dec:(bit dec) ~x:(b8 x) ~c:(bit c) in
            let st_a, rf = exec ~opcode ~a:0 ~b:x ~ac ~c in
            ignore st_a;
            check "incdec" ~inputs (res, f) (model.b, rf))
          [ (0, 0x04); (1, 0x05) ];
        for op = 0 to 3 do
          let res, c' = A.rotate ~op:(Bits.of_int ~width:2 op) ~a:(b8 x) ~c:(bit c) in
          check "rotate" ~inputs
            (res, { (flags_of ~ac ~c) with c = c' })
            (exec ~opcode:(0x07 lor (op lsl 3)) ~a:x ~b:0 ~ac ~c)
        done;
        check "daa" ~inputs
          (A.daa ~a:(b8 x) ~ac:(bit ac) ~c:(bit c))
          (exec ~opcode:0x27 ~a:x ~b:0 ~ac ~c)
      done
    done
  done;
  report "incdec, rotate, daa";
  [%expect {| incdec, rotate, daa: 7168 cases, 0 differences |}]

let%expect_test "the flag byte packs and unpacks" =
  let ok = ref true in
  for x = 0 to 255 do
    if Bits.to_int (A.pack (A.unpack (b8 x))) <> x land 0xd5 lor 0x02 then ok := false
  done;
  Printf.printf "%b\n" !ok;
  [%expect {| true |}]
