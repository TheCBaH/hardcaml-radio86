(* The arithmetic of the 8080 as combinational functions over any [Comb.S]: [Core] instantiates them on
   signals, the tests evaluate the same code on [Bits] for every input and compare with Rivet's model. *)
module Make (B : Hardcaml.Comb.S) = struct
  open B

  type flags = { s : t; z : t; ac : t; p : t; c : t }

  let parity x = ~:(reduce ~f:( ^: ) (bits_msb x))
  let of_result res ~ac ~c = { s = msb res; z = res ==:. 0; ac; p = parity res; c }

  (* the flag byte as PUSH PSW stores it *)
  let pack f = concat_msb [ f.s; f.z; gnd; f.ac; gnd; f.p; vdd; f.c ]
  let unpack x = { s = x.:(7); z = x.:(6); ac = x.:(4); p = x.:(2); c = x.:(0) }

  (* ADD ADC SUB SBB ANA XRA ORA CMP, [op] being bits 5..3 of the opcode. A subtraction adds the
     complement: its carry is the inverted carry out, its auxiliary carry is not inverted. ANA's
     auxiliary carry is the OR of bits 3 (8080, not 8085). The result of CMP is the difference. *)
  let alu ~op ~a ~y ~c =
    let is n = op ==:. n in
    let sub = is 2 |: is 3 |: is 7 and logic = is 4 |: is 5 |: is 6 in
    let cin = mux op [ gnd; c; vdd; ~:c; gnd; gnd; gnd; vdd ] in
    let yin = mux2 sub ~:y y in
    let sum9 = uresize a 9 +: uresize yin 9 +: uresize cin 9 in
    let sum = sum9.:[7, 0] in
    let res = mux op [ sum; sum; sum; sum; a &: y; a ^: y; a |: y; sum ] in
    let sum_ac = a.:(4) ^: yin.:(4) ^: sum.:(4) in
    let ac = mux2 (is 4) (a.:(3) |: y.:(3)) (mux2 logic gnd sum_ac) in
    let c = mux2 logic gnd (mux2 sub ~:(sum9.:(8)) sum9.:(8)) in
    (res, of_result res ~ac ~c)

  (* INR and DCR: the carry is kept *)
  let incdec ~dec ~x ~c =
    let res = mux2 dec (x -:. 1) (x +:. 1) in
    let ac = mux2 dec ~:(x.:[3, 0] ==:. 0) (x.:[3, 0] ==:. 0xf) in
    (res, of_result res ~ac ~c)

  (* RLC RRC RAL RAR, [op] being bits 4..3 of the opcode: the result and the new carry *)
  let rotate ~op ~a ~c =
    let left = a.:(7) and right = a.:(0) in
    ( mux op [ a.:[6, 0] @: left; right @: a.:[7, 1]; a.:[6, 0] @: c; c @: a.:[7, 1] ],
      mux op [ left; right; left; right ] )

  let daa ~a ~ac ~c =
    (* the low nibble carries into the high one when it is above 9: then 6 is added and it overflows *)
    let over = a.:[3, 0] >:. 9 in
    let lo = over |: ac and hi = a >:. 0x99 |: c in
    let cor = mux2 hi (of_int ~width:4 6) (zero 4) @: mux2 lo (of_int ~width:4 6) (zero 4) in
    let res = a +: cor in
    (res, of_result res ~ac:over ~c:hi)
end
