(* The pi spigot, ported to Rivet's OCaml builders.

   Origin:   litwr2/rosetta-pi-spigot, 8080/pi-8080.asm
             https://github.com/litwr2/rosetta-pi-spigot (5ec0f5369fd8c5439d76eb5acdd28386ca1ac246)
   Copy:     upstream/litwr2-pi-spigot/pi-8080.asm (the algorithm is the C program in its first lines:
             https://crypto.stanford.edu/pbc/notes/pi/code.html)
   Author:   litwr2 (the source credits ivagor, blackmirror, tricky and BigEd)
   Licence:  GNU General Public License 2.0 (upstream/litwr2-pi-spigot/LICENSE). This file
             is a translation of that text and carries the same licence, not the MIT licence
             of the rest of the repository.

   Ported: the computation of N = 7 * digits / 2 (label l1), the filling of the array r with 2000
   (lf0), the main loop (loop, l4, loop2, with the division inlined at the division macro), the
   conversion of each group of four digits (PR0000, PR0, PRD) and the variables cv and kv. The
   configuration is the generic one (GENERIC = 1, BIOS_OUTPUT = 0, MINUS = 1).

   Changes from the original, and nothing else:
   - the instruction syntax: the Zilog mnemonics of the Pasmo source become the 8080's
     (ld (x),hl = shld x, ld hl,(x) = lhld x, ex de,hl = xchg, add a,(hl) = add m, ld d,(hl) =
     mov d,m, jp nc,$+4 over one byte = a jnc to a label, push af = push psw, rra = rar ...);
   - removed as platform services: the program start (stack setup, the maximum from the BDOS
     address, the question and the answer), the timer, the output of the elapsed time, the
     keyboard and the exit; the program is entered at pi_start with HL = the number of digits (a
     multiple of four) and the stack set, and ends by jumping to showtimer, which the caller
     defines;
   - the character output of PRD (ld e,a / ld c,2 / call BDOS) is mov e,a / call putc, and the caller
     defines putc;
   - the labels are lower-case where the source's were capitals (pr0000, pr0, prd); the memory
     they use (ra, m10000) is laid out by the caller as in the source: m10000 on a page boundary,
     ra just after its three pages;
   - the self-modified operands are kept: svde+1, m1+1 and the operands inside the division.

   Contract: after the run the array r is garbage and the digits are the characters that putc
   received, in order; the first group is 3141. *)

let routine ~fresh =
  let open I8080_asm.Items in
  let sym = Pi_div.sym in
  [
    (* the number of loop passes: N = digits * 7 / 2 *)
    label "pi_start";
    mov d h;
    mov e l;
    dad hl;
    dad de;
    dad hl;
    dad de;
    mov a h;
    rar ();
    mov h a;
    mov a l;
    rar ();
    mov l a;
    push hl;
    (* fill the array r with 2000 *)
    pop hl;
    shld_e (symbol "kv");
    dcx hl;
    mov a l;
    cma ();
    mov c a;
    mov a h;
    cma ();
    mov b a;
    lxi de 2000;
    lxi_e hl (sym ~off:2 "ra");
    label "lf0";
    mov m e;
    inr l;
    mov m d;
    inx hl;
    inr c;
    jnz_e (symbol "lf0");
    inr b;
    jnz_e (symbol "lf0");
    mov l b;
    mov h b;
    shld_e (symbol "cv");
    (* the main loop *)
    label "loop";
    lhld_e (symbol "kv");
    dad hl;
    push hl;
    lxi_e bc (symbol "ra");
    dad bc;
    lxi bc 0;
    push bc;
    push bc;
    jmp_e (symbol "loop2");
    label "l4";
    dad de;
    jnc_e (symbol "pi_s1");
    inx bc;
    label "pi_s1";
    xchg ();
    label "svde";
    lxi hl 0;
    mov a l;
    sub e;
    mov l a;
    mov a h;
    sbb d;
    mov h a;
    xchg ();
    pop hl;
    mov a l;
    sbb c;
    mov l a;
    mov a h;
    sbb b;
    rar ();
    mov h a;
    mov a l;
    rar ();
    mov l a;
    mov a d;
    rar ();
    mov d a;
    mov a e;
    rar ();
    mov e a;
    push hl;
    push de;
    lhld_e (sym ~off:1 "m1");
    dcx hl;
    dcr l;
    label "loop2";
    shld_e (sym ~off:1 "m1");
    mov d m;
    inr l;
    mov l m;
    mvi_e h (plus (const 2) (high (symbol "m10000")));
    mov b m;
    dcr h;
    mov c m;
    dcr h;
    mov a m;
    mov l d;
    mov e m;
    inr h;
    add m;
    mov d a;
    inr h;
    mov a m;
    adc c;
    mov c a;
    jnc_e (symbol "pi_s2");
    inr b;
    label "pi_s2";
    pop hl;
    dad de;
    shld_e (sym ~off:1 "svde");
    xchg ();
    pop hl;
    jnc_e (symbol "pi_s3");
    inx hl;
    label "pi_s3";
    dad bc;
    pop bc;
    dcx bc;
    dcr c;
    push bc;
    mov a b;
    push hl;
    push psw;
  ]
  (* div32x16: BCDE = HLDE / BC, HL = HLDE mod BC *)
  @ Pi_div.div32x16 ~fresh
  @ [
      label "m1";
      shld 0;
      pop psw;
      jnz_e (symbol "l4");
      ora a;
      jnz_e (symbol "l4");
      pop hl;
      pop hl;
      (* the four digits: c + d / 10000, and c <- d mod 10000 *)
      mov h b;
      mov l c;
      lxi bc 10000;
      call_e (symbol "div32x16r");
      lda_e (symbol "cv");
      mov c a;
      lda_e (plus (symbol "cv") (const 1));
      shld_e (symbol "cv");
      mov h a;
      mov l c;
      dad de;
      call_e (symbol "pr0000");
      (* k <- k - 14, until it is 0 *)
      lhld_e (symbol "kv");
      lxi de (-14);
      dad de;
      mov a h;
      ora l;
      jz_e (symbol "showtimer");
      shld_e (symbol "kv");
      jmp_e (symbol "loop");
      (* HL as four decimal digits *)
      label "pr0000";
      lxi de (-1000);
      call_e (symbol "pr0");
      lxi de (-100);
      call_e (symbol "pr0");
      lxi de (-10);
      call_e (symbol "pr0");
      mov a l;
      label "prd";
      adi 0x30;
      push hl;
      mov e a;
      call_e (symbol "putc");
      pop hl;
      ret ();
      label "pr0";
      mvi a 0xff;
      label "pr0_1";
      mov b h;
      mov c l;
      inr a;
      dad de;
      jc_e (symbol "pr0_1");
      mov h b;
      mov l c;
      jmp_e (symbol "prd");
      label "cv";
      db [ 0; 0 ];
      label "kv";
      db [ 0; 0 ];
    ]
