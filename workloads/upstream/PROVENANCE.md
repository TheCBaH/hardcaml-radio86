# Third-party 8080 sources

Verbatim copies of the 8080 code that the OCaml programs in `../lib` are ported from, with the licence each
upstream carries. They are reference material: nothing builds or assembles them. They are kept beside the
ports so that every adaptation can be read against its original. The licences of the copies are not the
licence of the rest of this repository (MIT, `LICENSE.md`).

| Directory | Upstream | Revision | Licence |
| --- | --- | --- | --- |
| `rosettacode-ackermann/` | [Rosetta Code, Ackermann function, 8080 Assembly](https://rosettacode.org/wiki/Ackermann_function#8080_Assembly), as preserved in [acmeism/RosettaCodeData](https://github.com/acmeism/RosettaCodeData/blob/1d475861d7141ef2fafff293d9339f56e4b0f5db/Task/Ackermann-function/8080-Assembly/ackermann-function.8080) | RosettaCodeData `1d475861d7141ef2fafff293d9339f56e4b0f5db` (file last changed in `f23f22d71c285985898c796573a9c514cc8ea505`, 2023-07-01) | GNU Free Documentation License 1.2 (`LICENSE-GFDL-1.2.txt`; the terms of [Rosetta Code:Copyrights](https://rosettacode.org/wiki/Rosetta_Code:Copyrights)). The author is the contributor of the 8080 section of the task page, see its history; RosettaCodeData carries no per-file author or licence record. |
| `litwr2-pi-spigot/` | [litwr2/rosetta-pi-spigot](https://github.com/litwr2/rosetta-pi-spigot), directory `8080` | `5ec0f5369fd8c5439d76eb5acdd28386ca1ac246` | GNU General Public License 2.0 (`LICENSE`). The author is litwr2; the source credits ivagor, blackmirror, tricky and BigEd for the division idea and help. |

SHA-256 of the copies:

```
f0c52d24c96de0e28bce0740007293968f62abe50648ae0a7c6f46c982fe0df8  rosettacode-ackermann/ackermann-function.8080
2652c22ba086f92e55ae4a9f9c890ad4766ffd7814a73d318e22a597edf857a4  rosettacode-ackermann/LICENSE-GFDL-1.2.txt
6525711de5cd5fa4caa68393160d536de3a70e0adabb5461253a13948f898788  litwr2-pi-spigot/8080-mul16.s
e5b03341dd4141953de440056776d814dbed7489c0609d81a8cbe805984c686c  litwr2-pi-spigot/pi-8080.asm
239a0d63a7e174b4a92522aa4759aff841ee0a82d6996210c0a432407777c55b  litwr2-pi-spigot/8080-div.s
4ef24c61dc3c6ea80cd134f72b7ca5fc147b1f3bbb71ce72b1179f2d14782ac4  litwr2-pi-spigot/8080-div8.s
8177f97513213526df2cf6184d8ff986c675afb514d4e68a404010521b880643  litwr2-pi-spigot/LICENSE
```

`ackermann-function.8080` is not the demonstration's CP/M part used here: only the `ack`, `ackm` and `ackmn`
routine is ported. `8080-mul16.s` and the `div32x16r` routine of `pi-8080.asm` are written in the Zilog
mnemonics of the Pasmo assembler (`ld`, `jp`, `ex de,hl`) for the 8080; the ports spell the same instructions
with the 8080's.

## Ports

Each port is one file in `../lib`, headed with its origin, licence, the mapping of labels and the list of
changes; the files authored here (oracles, fixtures, drivers, runners) say so and are MIT.

| Port | Origin | Licence of the port |
| --- | --- | --- |
| `ackermann.ml` | `rosettacode-ackermann/ackermann-function.8080`, labels `ack` to `ackmn` | GFDL 1.2 |
| `mul16.ml` | `litwr2-pi-spigot/8080-mul16.s` | GPL-2.0 |
| `div32x16r.ml` | `litwr2-pi-spigot/pi-8080.asm`, routine `div32x16r` (lines 617-654) | GPL-2.0 |
