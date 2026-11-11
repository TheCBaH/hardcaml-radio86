(* The 32 by 16-bit division of the pi spigot, ported to Rivet's OCaml builders.

   Origin:   litwr2/rosetta-pi-spigot, 8080/8080-div.s
             https://github.com/litwr2/rosetta-pi-spigot (5ec0f5369fd8c5439d76eb5acdd28386ca1ac246)
   Copy:     upstream/litwr2-pi-spigot/8080-div.s
   Author:   litwr2 (the source credits ivagor, blackmirror, tricky and BigEd)
   Licence:  GNU General Public License 2.0 (upstream/litwr2-pi-spigot/LICENSE). This file
             is a translation of that text and carries the same licence, not the MIT licence
             of the rest of the repository.

   Changes from the original, and nothing else:
   - the instruction syntax: the Zilog mnemonics of the Pasmo source become the 8080's
     (adc a,a = adc a, jp c = jc, ld (x),a = sta x, ld a,(x) = lda x, scf = stc, inc c = inr c,
     ld bc,0 = lxi b,0, cpl = cma, xor a = xra a, sub c, add hl,hl = dad h);
   - the macros div1x, div1xs, divzssz, divzasz, divze, divxxz and div32x16 are functions
     returning items; the [local] labels of each use are made unique by [fresh];
   - macro arguments that are label expressions (svhl+1, mz2+2, svde+1 ...) are Expr values;
   - the include of 8080-div8.s is Pi_div8.routine, in the same place;
   - the labels div32x8, divminus, DIV320, mz1..mz3, svhl, enddivision and enddivision1 are
     lower-case (div320), the others keep their names.

   Contract of the div32x16 expansion (the source's comment): BCDE = HLDE / BC, HL = HLDE mod BC,
   BC odd after its [inc c]; it is wrong if BC > 0x7fff and HL >= BC. The caller (Pi_spigot)
   defines svde, whose operand bytes svde+1 and svde+2 the expansion reads as the low and high bytes
   of DE, and the label m1 that follows the expansion. The expansion writes its own operands
   (svhl, mz1+1, mz2+1, mz2+2, mz3+1): code and data that Pi_spigot declares writable. *)

let sym ?(off = 0) name =
  if off = 0 then I8080_asm.symbol name else I8080_asm.(plus (symbol name) (const off))

let div1x ~fresh px py =
  let open I8080_asm.Items in
  let t1 = fresh "div1x_t1" and t2 = fresh "div1x_t2" in
  [
    adc a;
    jc_e (symbol px);
    dad hl;
    label py;
    jc_e (symbol t1);
    mov e a;
    mov a l;
    add c;
    mov a h;
    adc b;
    mov a e;
    jnc_e (symbol t2);
    label t1;
    dad bc;
    stc ();
    label t2;
  ]

let div1xs px py = I8080_asm.Items.[ label px; dad hl; inr l; jmp_e (symbol py) ]
let divzssz px = I8080_asm.Items.[ adc a; dad hl; dad de; jnc_e (symbol px) ]
let divzasz px = I8080_asm.Items.[ adc a; dad hl; dad bc; jc_e (symbol px) ]

(* divze sz: the sixteen-step loop unrolled as two chains of eight, one for each carry state. *)
let divze ~fresh =
  let open I8080_asm.Items in
  let n prefix k = fresh (Printf.sprintf "divze_%s%d" prefix k) in
  let s = Array.init 9 (fun k -> if k = 0 then "" else n "S" k)
  and a_ = Array.init 9 (fun k -> if k = 0 then "" else n "A" k)
  and l1 = fresh "divze_l1" in
  [ dad hl; dad bc; jc_e (symbol s.(1)) ]
  @ List.concat (List.init 7 (fun i -> label a_.(i + 1) :: divzasz s.(i + 2)))
  @ [ label a_.(8); adc a; jmp_e (symbol l1) ]
  @ List.concat (List.init 7 (fun i -> label s.(i + 1) :: divzssz a_.(i + 2)))
  @ [ label s.(8); adc a; dad de; label l1 ]

let divxxz ~fresh ~pxh ~pxl ~pxr =
  let open I8080_asm.Items in
  let l1 = fresh "divxxz_l1" and l2 = fresh "divxxz_l2" and l3 = fresh "divxxz_l3" in
  [ dad de ]
  @ divze ~fresh
  @ [
      sta_e pxr;
      lda_e pxh;
      add l;
      mov l a;
      jnc_e (symbol l1);
      inr h;
      jnz_e (symbol l1);
      dad de;
      lda_e pxr;
      inr a;
      sta_e pxr;
      label l1;
    ]
  @ divze ~fresh
  @ [
      mov e a;
      lda_e pxl;
      add l;
      mov l a;
      jnc_e (symbol l3);
      inr h;
      jnz_e (symbol l3);
      inr e;
      jmp_e (symbol l2);
      label l3;
      dad bc;
      label l2;
    ]

(* The expansion of the div32x16 macro, with MINUS = 1 as in the source. *)
let div32x16 ~fresh =
  let open I8080_asm.Items in
  let lbl = symbol in
  let q = Pi_div8.routine ~fresh in
  let long_part =
    [ shld_e (lbl "svhl"); lxi hl 0 ]
    @ divxxz ~fresh ~pxh:(sym ~off:1 "svhl") ~pxl:(lbl "svhl") ~pxr:(sym ~off:2 "mz2")
    @ [ mov a e; sta_e (sym ~off:1 "mz2"); xra a; sub c; mov e a ]
    @ divxxz ~fresh ~pxh:(sym ~off:2 "svde") ~pxl:(sym ~off:1 "svde") ~pxr:(sym ~off:1 "mz1")
    @ [
        label "mz1";
        mvi d 0;
        label "mz2";
        lxi bc 0;
        jmp_e (lbl "enddivision");
        label "svhl";
        db [ 0; 0 ];
      ]
  in
  let divminus =
    let half names = List.concat_map (fun (px, py) -> div1x ~fresh px py) names in
    let xs = List.init 8 (fun k -> (Printf.sprintf "lx%d" k, Printf.sprintf "ly%d" k))
    and zs = List.init 8 (fun k -> (Printf.sprintf "lz%d" k, Printf.sprintf "lw%d" k)) in
    [ label "divminus"; mov c a; mov a b; cma (); mov b a; mov a d ]
    @ half xs
    @ [ adc a; mov d a; lda_e (sym ~off:1 "svde") ]
    @ half zs
    @ [ adc a; mov e a; jmp_e (lbl "enddivision1") ]
    @ List.concat_map (fun (px, py) -> div1xs px py) (xs @ zs)
  in
  [
    ora a;
    jz_e (lbl "div32x8");
    mov a c;
    cma ();
    jm_e (lbl "divminus");
    mov e a;
    add l;
    mov a b;
    cma ();
    mov d a;
    adc h;
    inr c;
    jnc_e (lbl "div320");
  ]
  @ long_part
  @ [ label "div32x8"; inr c; jm_e (lbl "div32x8e") ]
  @ q @ divminus
  @ [ label "div320" ]
  @ divxxz ~fresh ~pxh:(sym ~off:2 "svde") ~pxl:(sym ~off:1 "svde") ~pxr:(sym ~off:1 "mz3")
  @ [ label "mz3"; mvi d 0; label "enddivision1"; lxi bc 0; label "enddivision" ]
