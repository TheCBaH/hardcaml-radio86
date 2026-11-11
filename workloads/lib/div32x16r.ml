(* 32 by 16-bit division, ported to Rivet's OCaml builders.

   Origin:   litwr2/rosetta-pi-spigot, 8080/pi-8080.asm, routine div32x16r (lines 617-654)
             https://github.com/litwr2/rosetta-pi-spigot (5ec0f5369fd8c5439d76eb5acdd28386ca1ac246)
   Copy:     upstream/litwr2-pi-spigot/pi-8080.asm
   Author:   litwr2 (the source credits ivagor, blackmirror, tricky and BigEd)
   Licence:  GNU General Public License 2.0 (upstream/litwr2-pi-spigot/LICENSE). This file
             is a translation of that text and carries the same licence, not the MIT licence
             of the rest of the repository.

   Changes from the original, and nothing else:
   - the instruction syntax: the Zilog mnemonics of the Pasmo source become the 8080's
     (dec bc = dcx b, cpl = cma, ex de,hl = xchg, add hl,hl = dad h, add hl,bc = dad b,
     jp c = jc, ret nc = rnc, inc e = inr e, ld bc,0 = lxi b,0);
   - the [proc]/[local] labels t, t0..t4, l1 and l2 become div32x16r_t, _t0 .. _t4, _l1, _l2.

   Contract: HL:DE is the dividend, BC a nonzero divisor, with HL < BC so the quotient fits.
   The quotient is in DE, the remainder in HL, and BC is 0. Destroys A. The nested calls t0..t3
   are the loop: they run the shift-and-subtract step sixteen times and need ten bytes of stack. *)

let routine =
  let open I8080_asm in
  [
    label "div32x16r";
    insn (call_e (symbol "div32x16r_t"));
    insn (lxi bc 0);
    insn (ret ());
    label "div32x16r_t";
    insn (dcx bc);
    insn (mov a b);
    insn (cma ());
    insn (mov b a);
    insn (mov a c);
    insn (cma ());
    insn (mov c a);
    insn (call_e (symbol "div32x16r_t0"));
    label "div32x16r_t0";
    insn (call_e (symbol "div32x16r_t1"));
    label "div32x16r_t1";
    insn (call_e (symbol "div32x16r_t2"));
    label "div32x16r_t2";
    insn (call_e (symbol "div32x16r_t3"));
    label "div32x16r_t3";
    insn (xchg ());
    insn (dad hl);
    insn (xchg ());
    insn (jc_e (symbol "div32x16r_l1"));
    insn (dad hl);
    label "div32x16r_l2";
    insn (jc_e (symbol "div32x16r_t4"));
    insn (mov a l);
    insn (add c);
    insn (mov a h);
    insn (adc b);
    insn (rnc ());
    label "div32x16r_t4";
    insn (dad bc);
    insn (inr e);
    insn (ret ());
    label "div32x16r_l1";
    insn (dad hl);
    insn (inr l);
    insn (jmp_e (symbol "div32x16r_l2"));
  ]
