(* The Ackermann routine, ported to Rivet's OCaml builders.

   Origin:   Rosetta Code, "Ackermann function", 8080 Assembly
             https://rosettacode.org/wiki/Ackermann_function#8080_Assembly
   Copy:     upstream/rosettacode-ackermann/ackermann-function.8080
             (acmeism/RosettaCodeData 1d475861d7141ef2fafff293d9339f56e4b0f5db)
   Author:   the contributor of the 8080 section of that task page (see its history)
   Licence:  GNU Free Documentation License 1.2, as for all Rosetta Code content
             (upstream/rosettacode-ackermann/LICENSE-GFDL-1.2.txt). This file is a
             translation of that text and carries the same licence, not the MIT licence of
             the rest of the repository.

   Changes from the original, and nothing else:
   - the instruction syntax: Intel text becomes builder calls, [inx h] is [inx hl];
   - labels keep their names: ack, ackm, ackmn;
   - the CP/M demonstration (org 100h, jmp demo, the table, prhl and its strings) is not
     ported; Fixtures supplies a driver that reads (M, N) pairs from RAM.

   Contract: DE = M, HL = N on entry; HL = A(M, N) on return. Destroys A, DE and HL, keeps BC.
   The arithmetic is 16-bit, so the result must fit 16 bits. The stack holds one word per
   pending non-tail call. *)

let routine =
  let open I8080_asm in
  [
    label "ack";
    insn (mov a d);
    insn (ora e);
    insn (jnz_e (symbol "ackm"));
    insn (inx hl);
    insn (ret ());
    label "ackm";
    insn (mov a h);
    insn (ora l);
    insn (jnz_e (symbol "ackmn"));
    insn (lxi hl 1);
    insn (dcx de);
    insn (jmp_e (symbol "ack"));
    label "ackmn";
    insn (push de);
    insn (dcx hl);
    insn (call_e (symbol "ack"));
    insn (pop de);
    insn (dcx de);
    insn (jmp_e (symbol "ack"));
  ]
