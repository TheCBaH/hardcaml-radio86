# Math workloads

Small, deterministic i8080 programs that run from reset to `HLT` with inputs and results in RAM: no console,
shim, boot preamble, timer or peripheral. They are built in OCaml with Rivet's `I8080_asm` builders (no
assembly text, no assembler process), checked against independent OCaml oracles, and run on three CPUs:
Rivet's behavioural model, the Hardcaml core and the released `vm80a_sim`.

| Path | What it is | Licence |
| --- | --- | --- |
| `upstream/` | Verbatim third-party 8080 sources the ports come from, each with its licence, revision and SHA-256 (`PROVENANCE.md`). Reference only; nothing builds them. | each upstream's own |
| `lib/ackermann.ml`, `mul16.ml`, `div32x16r.ml` | One port per routine, one file each. The header names the origin, author, licence, the label mapping and every change. | GFDL 1.2, GPL-2.0, GPL-2.0 |
| `lib/oracle.ml` | Reference answers in plain OCaml: Ackermann (by definition and in closed form), 16-bit product and 32/16 division in `Int64`, digits of pi by Machin's formula. | MIT |
| `lib/fixture.ml`, `fixtures.ml`, `model_run.ml` | A workload as data (code image, RAM image, writable regions, expected RAM, budget), our own drivers that read cases from RAM and write answers to RAM, and the runner in Rivet's model. | MIT |
| `bin/workload_files.ml` | Writes the fixtures as files for an external CPU. | MIT |
| `test/` | Oracle checks, the fixtures in the model, planted failures, random operands. | MIT |

`local/test/test_workloads.ml` runs the same fixtures on the Hardcaml core and replays them in lockstep with
Rivet's model; `thirdparty/vm80a/scripts/workloads.sh` runs them on `vm80a_sim` (`make workloads-vm80a`).

## Memory map

Code at 0, so that reset enters it. Data image at `0x4000`: guard, input table, zeroed results, scratch
variables (including `sp_save`), guard; the guards are eight bytes of `0xa5`. The stack grows down from
`0xf000` in a 1 KiB region with a guard under it. A run may write only the results, the scratch variables and
the stack; every driver ends with `SP` stored in `sp_save`, then `HLT`.

## What is checked

1. `HLT` reached, no trap, within the T-state budget, no `IN` or `OUT`.
2. Every result word equals the oracle's, and `sp_save` holds the initial stack pointer.
3. Every write lies in a declared region and the three guards are intact.
4. The Hardcaml core, Rivet's model and `vm80a_sim` leave the same 64 KiB and count the same T-states; the
   core is also replayed instruction by instruction against the model.

## Workloads

| Fixture | Routine | Cases | Code | Data | T-states | Stack used |
| --- | --- | --- | --- | --- | --- | --- |
| `ackermann` | Rosetta Code `ack` | (0,0) (0,7) (1,0) (1,4) (2,0) (2,4) (3,2) | 86 B | 66 B | 41782 | 110 B |
| `multiply` | litwr2 `mul16`, low 16 bits of HL * BC | 15: zero, one, alternating bits, around `0x8000`, `0xffff` | 90 B | 113 B | 12919 | 2 B |
| `divide` | litwr2 `div32x16r`, HLDE / BC with HL < BC | 16: zero dividend, divisor one, exact, inexact, high-bit divisors, quotient `0xffff` | 135 B | 183 B | 34356 | 12 B |

Budgets are the measured T-states plus about 20 %: 50 000, 16 000 and 42 000.
