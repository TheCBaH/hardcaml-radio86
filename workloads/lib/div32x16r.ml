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
  let open I8080_asm.Items in
  [
    label "div32x16r";
    call_e (symbol "div32x16r_t");
    lxi bc 0;
    ret ();
    label "div32x16r_t";
    dcx bc;
    mov a b;
    cma ();
    mov b a;
    mov a c;
    cma ();
    mov c a;
    call_e (symbol "div32x16r_t0");
    label "div32x16r_t0";
    call_e (symbol "div32x16r_t1");
    label "div32x16r_t1";
    call_e (symbol "div32x16r_t2");
    label "div32x16r_t2";
    call_e (symbol "div32x16r_t3");
    label "div32x16r_t3";
    xchg ();
    dad hl;
    xchg ();
    jc_e (symbol "div32x16r_l1");
    dad hl;
    label "div32x16r_l2";
    jc_e (symbol "div32x16r_t4");
    mov a l;
    add c;
    mov a h;
    adc b;
    rnc ();
    label "div32x16r_t4";
    dad bc;
    inr e;
    ret ();
    label "div32x16r_l1";
    dad hl;
    inr l;
    jmp_e (symbol "div32x16r_l2");
  ]
