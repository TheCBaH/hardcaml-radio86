# hardcaml-radio86

[![build](https://github.com/TheCBaH/hardcaml-radio86/actions/workflows/build.yml/badge.svg?branch=devel)](https://github.com/TheCBaH/hardcaml-radio86/actions/workflows/build.yml)

An Intel 8080 (KR580VM80A) for the Radio-86RK, in Hardcaml. This repository starts with the
**observation** side: reading waveforms of an existing 8080 core and turning them into bus transfers,
instructions and register state, checked against an independent behavioural model. The Hardcaml CPU
(goal P) is added beside it and observed with the same library.

`i8080_trace` (library) and `i8080_observe` (command):

| Layer | What it does |
| --- | --- |
| `Manifest` | Versioned JSON (`i8080-observation-manifest/1`): clock, sample rule and enable, and the path, width and kind (`pin` or `register`) of every pin and internal register. A path the trace lacks, or a width that differs, is a hard error that names every such probe. |
| `Observe` | Binds the manifest to a VCD or FST with [hwfrp](https://github.com/TheCBaH/coralview/tree/main/hwfrp) and samples one `Frame` per T-state. |
| `Bus` | Machine cycles from the pins: accepted transfers (not strobes), waits, reset epochs, halt acknowledge. Cycles that do not finish are reported as such. |
| `Instr`, `Regs` | Instruction spans decoded with Rivet's i8080 decoder, and register snapshots from the core's flops. |
| `Lockstep` | The observed instructions replayed one by one in Rivet's behavioural model: PC, bytes, ordered data accesses, registers and T-states. |
| `Events`, `Perfetto`, `Export` | JSON lines, text and Perfetto lanes, and an export directory that `replay` reproduces byte for byte. |

Unknown data (X or Z in the waveform, a masked cell in a hand-written trace) is carried as unknown and
reported; it is never turned into zero.

```sh
i8080_observe check    --manifest manifests/vm80a-fst.json --trace run.fst
i8080_observe observe  --manifest manifests/vm80a-fst.json --trace run.fst --print --export out/run
i8080_observe replay   out/run
i8080_observe lockstep --manifest manifests/vm80a-fst.json --trace pre.fst --program 8080PRE.COM --shim <dir>
i8080_observe k4       --manifest manifests/vm80a-fst.json --hand mvi-sta-hlt.trace --against run.fst
```

The manifest format and what was found reading vm80a are in [docs/manifest.md](docs/manifest.md).

## Pins, nothing built

- hwfrp (coralview `e78a32b`) and Rivet (`932dcbd`) are opam pins, in `.devcontainer/devcontainer.json`
  and `hardcaml-radio86.opam.template`. `make pin-check` keeps the Rivet commit in agreement with
  `scripts/rivet.pin`, from which `scripts/rivet-tool.sh` builds Rivet's seeded program generator.
- `vm80a_sim` and the exerciser images come from the
  [devcontainer.retrocpu-tools](https://github.com/TheCBaH/devcontainer.retrocpu-tools) release named in
  `scripts/retrocpu.pin`, by tag and SHA-256 (`scripts/retrocpu-fetch.sh` refuses anything else). No
  simulator, assembler or Verilator is built here.

## Tests

`make ci`: format check, build, unit tests (ppx_expect, no simulator needed), `observe-test` (the
MVI/STA/HLT fixtures against the K4 hand traces and the committed events, a manifest that names a missing
path, `8080PRE` in lockstep with planted model bugs, byte-exact replay) and `lockstep-test` (24 seeded
programs of 150 units under `vm80a_sim`, against the model; a planted bug must be caught).

`test/lockstep-known.txt` lists the differences between the core and the model that were investigated;
anything else stops the run.

## Licence

MIT. vm80a is CC BY 3.0 (1801BM1), used unmodified through the released `vm80a_sim`.
