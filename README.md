# hardcaml-radio86

[![build](https://github.com/TheCBaH/hardcaml-radio86/actions/workflows/build.yml/badge.svg?branch=devel)](https://github.com/TheCBaH/hardcaml-radio86/actions/workflows/build.yml)

An Intel 8080 (KR580VM80A) for the Radio-86RK, in Hardcaml. This repository starts with the
**observation** side: reading waveforms of an existing 8080 core and turning them into bus transfers,
instructions and register state, checked against an independent behavioural model. The Hardcaml CPU
(goal P) is added beside it and observed with the same library.

## Layout

| Directory | Holds | Depends on |
| --- | --- | --- |
| [`observatory/`](observatory/) | The common i8080 observation: `i8080_trace`, `i8080_observe`, the manifest format and the shared test support. | no RTL |
| [`thirdparty/vm80a/`](thirdparty/vm80a/) | What is specific to the third-party core: its manifest, the release fetch (pinned by SHA-256), the golden events, the lockstep known list and the simulator-driven tests. | the observatory |
| [`local/`](local/) | What is specific to the locally generated core: the Hardcaml RTL (`i8080_rtl`), its harness and `i8080_rtl_run`, its manifest and tests. | the observatory |

Tests follow the same split: `make test-observatory`, `make test-vm80a` and `make test-local` are the unit
tests of each part; `observe-test`, `lockstep-test` (third-party) and `rtl-test` (local) drive the
simulators. The common tests use their own synthetic bench (`observatory/test/reference.json`), not either
core's manifest.

## The observatory

`i8080_trace` (library) and `i8080_observe` (command):

| Layer | What it does |
| --- | --- |
| `Manifest` | Versioned JSON (`i8080-observation-manifest/1`): clock, sample rule and enable, and the path, width and kind (`pin` or `register`) of every pin and internal register. A path the trace lacks, or a width that differs, is a hard error that names every such probe. |
| `Observe` | Binds the manifest to a VCD or FST with [hwfrp](https://github.com/TheCBaH/coralview/tree/main/hwfrp) and samples one `Frame` per T-state. One `Bind.collect` pass names every probe that does not resolve; the run reports `Diag.summarize` (signals never driven, constant, unknown at the end). |
| `Discover` | Finds the manifest's probes in the trace's declarations, listed by ovcd (`Vcd_signals`, the lines of `vcd_dump --list-signals --format jsonl`): hints for a path that moved or is misspelt, and a re-pathed manifest when each probe has one candidate. |
| `Trail` | The executed instructions with the state each updated: changed registers, memory bytes written (with the byte they replaced when the capture showed it) and port transfers. |
| `Bus` | Machine cycles from the pins: accepted transfers (not strobes), waits, reset epochs, halt acknowledge. Cycles that do not finish are reported as such. |
| `Instr`, `Regs` | Instruction spans decoded with Rivet's i8080 decoder, and register snapshots from the core's flops. |
| `Lockstep` | The observed instructions replayed one by one in Rivet's behavioural model: PC, bytes, ordered data accesses, registers and T-states. |
| `Events`, `Perfetto`, `Export` | JSON lines, text and Perfetto lanes, and an export directory that `replay` reproduces byte for byte. |

Unknown data (X or Z in the waveform, a masked cell in a hand-written trace) is carried as unknown and
reported; it is never turned into zero.

```sh
i8080_observe check    --manifest thirdparty/vm80a/manifest.json --trace run.fst
i8080_observe observe  --manifest thirdparty/vm80a/manifest.json --trace run.fst --print --export out/run
i8080_observe replay   out/run
i8080_observe lockstep --manifest thirdparty/vm80a/manifest.json --trace pre.fst --program 8080PRE.COM --shim <dir>
i8080_observe k4       --manifest thirdparty/vm80a/manifest.json --hand mvi-sta-hlt.trace --against run.fst
i8080_observe trail    --manifest thirdparty/vm80a/manifest.json --trace run.fst
i8080_observe signals  --trace run.fst --like pin_
i8080_observe discover --manifest thirdparty/vm80a/manifest.json --trace other.fst --write moved.json
```

The manifest format is in [observatory/docs/manifest.md](observatory/docs/manifest.md); what was found reading vm80a is in
[thirdparty/vm80a/README.md](thirdparty/vm80a/README.md).

## The Hardcaml core (`local/`)

`i8080_rtl` (library, `local/rtl`) and `i8080_rtl_run` (command): the 8080 in Hardcaml v0.17.1 with **all 256
opcodes** and I/O, on one sequencing ROM built from `Plan`, the bus-visible machine cycles of each opcode.
`HOLD` and `INT` are not built: they stop the core in a fault state with a `trap_cause`, nothing is silently
ignored (no `INTA` cycle, no `HLDA`). One enabled cycle is one T-state (`tick`), the pins are a function of
the state only, and the observation ports (`sample`, `acc`, `bc`.. `pc`, `wz`, the flags, `tstate`, `cycle`,
`accept`, `opcode`, `trap`) carry the register and control state.

| Module | What it does |
| --- | --- |
| `Plan` | Per opcode: the machine cycles the pins show (status word, T-states, address source). DAD's idle cycles and the fifth state of PUSH, CALL, RST, Rcc and XTHL's last write are extra T-states of the cycle before, as on vm80a. Pure data, tested against Rivet's table. |
| `Core` | The datapath and sequencer (`local/rtl/core.ml`): register file, ALU, flags, DAA, stack, calls. A cycle's effects happen in its last T-state. |
| `Stimulus` | The K4 stimulus schedule: `ready`, `hold`, `int`, `reset` from a sample or a machine cycle (`m5+1`), `io` bytes per port and `inta` bytes. |
| `Harness` | Cyclesim around the core with 64 KiB of RAM and I/O ports: runs a program (optionally the K5 shim behind the release's boot preamble), stops at the halt cycle or after an OUT to a stop port, counts T-states (K2), records writes and OUTs and the K4 pin trace, and writes the `Vcd.wrap` waveform. |

The waveform is read by the same `i8080_observe` as the vm80a traces, with `local/manifest.json`:
the clock is `inputs.clock`, the sample strobe `outputs.sample`, **sampled after the rising edge** (see
`observatory/docs/manifest.md`), and `do` is qualified by `dout_en`. The fixture takes 27 T-states (29 with two waits)
with one write of `2a` to `2000`.

```sh
i8080_rtl_run run --hex --image mvi-sta-hlt.run.img --stim mvi-sta-hlt.stim --vcd run.vcd --pin-trace run.trace
i8080_rtl_run com 8080PRE.COM --vcd pre.vcd          # a CP/M .COM under the K5 shim
i8080_observe observe --manifest local/manifest.json --trace run.vcd --print
make rtl-test      # the fixtures, 8080PRE and io-console against vm80a_sim, replay, a faster clock
make rtl-lockstep-test   # Rivet's seeded programs on the core, in lockstep with the model
```

## Pins, nothing built

- hwfrp (coralview `5ff364d`, on `main`) and Rivet (`932dcbd`) are opam pins, in `.devcontainer/devcontainer.json`
  and `hardcaml-radio86.opam.template`. `make pin-check` keeps the Rivet commit in agreement with
  `scripts/rivet.pin`, from which `scripts/rivet-tool.sh` builds Rivet's seeded program generator.
- `vm80a_sim` and the exerciser images come from the
  [devcontainer.retrocpu-tools](https://github.com/TheCBaH/devcontainer.retrocpu-tools) release named in
  `scripts/retrocpu.pin`, by tag and SHA-256 (`scripts/retrocpu-fetch.sh` refuses anything else). No
  simulator, assembler or Verilator is built here.

## Tests

`make ci`: format check, build, unit tests (ppx_expect, no simulator needed; they include all 256 opcodes of the
core, from three starting states, replayed in Rivet's model), `observe-test` (the
MVI/STA/HLT fixtures against the K4 hand traces and the committed events, a manifest that names a missing
path, `8080PRE` in lockstep with planted model bugs, byte-exact replay) and `lockstep-test` (24 seeded
programs of 150 units under `vm80a_sim`, against the model; a planted bug must be caught), `rtl-test` (the Hardcaml
core's fixtures, then `io-console` and `8080PRE` run on both cores: the same OUTs, T-states, bus transfers and
instructions) and `rtl-lockstep-test` (the same seeded programs on the Hardcaml core).

`observatory/lockstep-known.txt` lists the differences between the core and the model that were investigated;
anything else stops the run.

## Licence

MIT. vm80a is CC BY 3.0 (1801BM1), used unmodified through the released `vm80a_sim`.
