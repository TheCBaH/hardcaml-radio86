(* 16-bit multiplication, ported to Rivet's OCaml builders.

   Origin:   litwr2/rosetta-pi-spigot, 8080/8080-mul16.s
             https://github.com/litwr2/rosetta-pi-spigot (5ec0f5369fd8c5439d76eb5acdd28386ca1ac246)
   Copy:     upstream/litwr2-pi-spigot/8080-mul16.s
   Author:   litwr2
   Licence:  GNU General Public License 2.0 (upstream/litwr2-pi-spigot/LICENSE). This file
             is a translation of that text and carries the same licence, not the MIT licence
             of the rest of the repository.

   Changes from the original, and nothing else:
   - the instruction syntax: the Zilog mnemonics of the Pasmo source become the 8080's
     (xor a = xra a, ld d,a = mov d,a, rra = rar, jp nc = jnc, ex de,hl = xchg,
     add hl,de = dad d, add hl,hl = dad h, or l = ora l, ret z = rz);
   - the [proc]/[local] labels ll3 and ll4 become mul16_ll3 and mul16_ll4.

   Contract: DE = the low 16 bits of HL * BC. Destroys A, BC and HL. It is not a 32-bit
   multiplier: the high half of the product is lost. *)

let routine =
  let open I8080_asm.Items in
  [
    label "mul16";
    xra a;
    mov d a;
    mov e a;
    label "mul16_ll3";
    mov a b;
    rar ();
    mov b a;
    mov a c;
    rar ();
    mov c a;
    jnc_e (symbol "mul16_ll4");
    xchg ();
    dad de;
    xchg ();
    label "mul16_ll4";
    dad hl;
    mov a h;
    ora l;
    rz ();
    mov a b;
    ora c;
    jnz_e (symbol "mul16_ll3");
    ret ();
  ]
