(* The multiplication table of the pi spigot, ported to Rivet's OCaml builders.

   Origin:   litwr2/rosetta-pi-spigot, amstrad/cpc6128/mul10000.s (the 8080 source includes a
             file of this name; the 8080 directory does not hold it, the other platforms' copies
             that hold the data are identical)
             https://github.com/litwr2/rosetta-pi-spigot (5ec0f5369fd8c5439d76eb5acdd28386ca1ac246)
   Copy:     upstream/litwr2-pi-spigot/mul10000.s
   Author:   litwr2
   Licence:  GNU General Public License 2.0 (upstream/litwr2-pi-spigot/LICENSE). This file
             is a translation of that text and carries the same licence, not the MIT licence
             of the rest of the repository.

   Changes from the original, and nothing else:
   - the 48 lines of 16 bytes are produced by their definition instead of being written out;
     the unit test compares the bytes with the upstream file;
   - the label m10000 keeps its name.

   The table is three pages, each indexed by a byte n: the low, middle and high byte of
   n * 10000. The spigot's inner loop reads them with H set to the page, so the table must start
   on a page boundary (Fixtures pads to it). *)

let page byte = List.init 256 (fun n -> ((n * 10000) lsr (8 * byte)) land 0xff)
let bytes = List.concat [ page 0; page 1; page 2 ]
let routine = I8080_asm.[ label "m10000"; db bytes ]
