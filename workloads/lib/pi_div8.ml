(* Division by a divisor below 256, ported to Rivet's OCaml builders.

   Origin:   litwr2/rosetta-pi-spigot, 8080/8080-div8.s
             https://github.com/litwr2/rosetta-pi-spigot (5ec0f5369fd8c5439d76eb5acdd28386ca1ac246)
   Copy:     upstream/litwr2-pi-spigot/8080-div8.s
   Author:   litwr2 (the source credits ivagor, blackmirror, tricky and BigEd)
   Licence:  GNU General Public License 2.0 (upstream/litwr2-pi-spigot/LICENSE). This file
             is a translation of that text and carries the same licence, not the MIT licence
             of the rest of the repository.

   Changes from the original, and nothing else:
   - the instruction syntax: the Zilog mnemonics of the Pasmo source become the 8080's
     (cp c = cmp c, sub c, inc l = inr l, ld h,0 = mvi h,0, add hl,hl = dad h, jp = j..);
   - the macros div8i and div8j are functions, and [rept 8] is a list of eight copies; their
     [local] labels are made unique by [fresh];
   - the [proc]/[local] labels lx and l1 of div32x8f and div32x8e become div32x8f_lx and
     div32x8e_l1;
   - the jump out of both routines goes to the label [enddivision], which Pi_div defines.

   Contract (as in the source): B = 0, HL:DE the dividend, C the divisor; div32x8f is for C < 0x80
   and div32x8e for C > 0x7f. The quotient is in BCDE-order registers as Pi_div's macro expects.
   div32x8f must come first: Pi_div falls into it. *)

let div8i ~fresh =
  let open I8080_asm.Items in
  let l1 = fresh "div8i_l1" in
  [ dad hl; mov a h; sub c; jc_e (symbol l1); mov h a; inr l; label l1 ]

let div8j ~fresh =
  let open I8080_asm.Items in
  let l1 = fresh "div8j_l1" and l2 = fresh "div8j_l2" in
  [
    dad hl;
    mov a h;
    jc_e (symbol l1);
    cmp c;
    jc_e (symbol l2);
    label l1;
    sub c;
    mov h a;
    inr l;
    label l2;
  ]

let rept8 f = List.concat (List.init 8 (fun _ -> f ()))

let div32x8f ~fresh =
  let open I8080_asm.Items in
  [ label "div32x8f"; mov a h; cmp c; jc_e (symbol "div32x8f_lx"); mov b l; mov l h; mvi h 0 ]
  @ rept8 (fun () -> div8i ~fresh)
  @ [ mov a l; mov l b; mov b a; label "div32x8f_lx" ]
  @ rept8 (fun () -> div8i ~fresh)
  @ [ mov a l; mov l d; mov d a ]
  @ rept8 (fun () -> div8i ~fresh)
  @ [ mov a l; mov l e; mov e a ]
  @ rept8 (fun () -> div8i ~fresh)
  @ [ mov c d; mov d e; mov e l; mov l h; mvi h 0; jmp_e (symbol "enddivision") ]

let div32x8e ~fresh =
  let open I8080_asm.Items in
  [
    label "div32x8e";
    mov a h;
    cmp c;
    jc_e (symbol "div32x8e_l1");
    sub c;
    mov h a;
    inr b;
    label "div32x8e_l1";
  ]
  @ rept8 (fun () -> div8j ~fresh)
  @ [ mov a l; mov l d; mov d a ]
  @ rept8 (fun () -> div8j ~fresh)
  @ [ mov a l; mov l e; mov e a ]
  @ rept8 (fun () -> div8j ~fresh)
  @ [ mov c d; mov d e; mov e l; mov l h; mvi h 0; jmp_e (symbol "enddivision") ]

(* The whole file, in its order: div32x8f first. *)
let routine ~fresh = div32x8f ~fresh @ div32x8e ~fresh
