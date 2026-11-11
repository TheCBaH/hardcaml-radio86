# Locally generated RTL: the Hardcaml i8080

The CPU written in this repository, in Hardcaml (v0.17.1, OCaml 5.4.1), simulated in Cyclesim and observed
with the common observatory like any other core.

| Path | What it is |
| --- | --- |
| `rtl/` | `i8080_rtl`: `Plan` (the machine cycles of each opcode), `Alu` (the arithmetic, over any `Comb.S`), `Vectors` and `Bench` (the Verilog testbenches), `Core` (datapath and sequencer, all 256 opcodes), `Stimulus` (the K4 schedule, `io` and `inta` included), `Harness` (Cyclesim around the core with 64 KiB of RAM, ports and the K5 shim). |
| `trace/` | `i8080_rtl_trace`: the harness's samples as the observatory's frames (the pin trace of the run). |
| `bin/` | `i8080_rtl_run`: `run` a program, `com` a CP/M `.COM` under the K5 shim, `verilog`; writes the `Vcd.wrap` waveform and the K4 pin trace. |
| `manifest.json` | The observation manifest of the `Vcd.wrap` waveform (`hardcaml-vcd`). |
| `scripts/` | `rtl-test.sh` (the release's fixtures, `io-console` and `8080PRE` on the core and on vm80a_sim), `verilog-test.sh` (the emitted Verilog under Icarus Verilog, every clock cycle against Cyclesim) and `lockstep-programs.sh` (seeded programs against Rivet's model). |
| `test/` | Unit tests: the cycle plan against Rivet's table, the ALU on `Bits` for every input against Rivet's model, all 256 opcodes in lockstep with Rivet's model, the fixtures, the waveform and its observation. |

```sh
make test-local   # the unit tests
make rtl-test     # fixtures, 8080PRE and io-console against vm80a_sim, a faster clock, export and replay
make verilog-test # the emitted Verilog replays Cyclesim's runs: same outputs in every clock cycle
make rtl-lockstep-test
make exerciser-test # 8080EX1 on the emitted Verilog under Verilator (about half an hour)
```

## HOLD and INT

Both follow what `vm80a_sim` does, found by running it on stimulus schedules (see `rtl-test.sh` and the unit
tests):

| Input | Behaviour |
| --- | --- |
| `hold` | Taken in T2, or in the Tw that sees READY, never in T1. `hlda` rises with T3 of a read and one T-state after T3 of a write; the cycle runs to its end (T4, T5) and the core then idles until `hold` falls: `hlda` is still high in that T-state and the next one is T1. A `hold` that falls before the cycle ends takes `hlda` down with it and nothing idles. In the halt state `hlda` follows `hold` one T-state later. |
| `int_req` | Sampled in the last T-state of an instruction, with `inte` set; not at the end of `EI` or `DI`, and not while a HOLD is being acknowledged. The next cycle is the acknowledge: status `23`, PC neither sent on nor incremented during the whole supplied instruction, `inte` cleared, the other bytes of a supplied `CALL` read with status `03`. Out of a halt the acknowledge (`2b`) starts three T-states after the one that saw `int_req`. |

Known differences from vm80a, on purpose:

- vm80a's acknowledge cycles show `a3`/`ab` (MEMR set) and its status bit 0 is the level of the INT pin in
  any T1; the core gives the K2 table's `23`/`2b`, and `03` for the address bytes whether INT is still high
  or not. Hold `int_req` until the acknowledge cycle.
- vm80a stops for good when HOLD falls in the last T-state of the cycle that acknowledged it; the core goes
  on with T1.
- An instruction of one 4 T-state cycle supplied by INTA with INT still high is acknowledged again by vm80a;
  the core has cleared `inte` and does not.

RESET (`rst_in`) is the synchronous clear of every register and does not wait for `tick`; the emitted
Verilog has no other initial state, which is why the testbench compares from the second cycle on.

## The manifest of the Hardcaml core (`local/manifest.json`)

The core's Cyclesim waveform comes from `Hardcaml.Vcd.wrap`. It writes, at the timestamp of the rising edge
that ends cycle k, `clock=1` together with the **pre-edge** inputs and outputs of cycle k. The values of
the T-state therefore sit at the edge, not before it:

| Sample rule | What hwfrp reads |
| --- | --- |
| `after_rising` with `enable` = `outputs.sample` | The pins and inputs of the T-state that this edge ends. This is the rule of the manifest. |
| `before_rising` | The values of the previous cycle: the inputs (`ready`, `di`) are one T-state late. K2's remark that `Before_rising` fits a strobe-qualified Cyclesim sample is true for registers updated by the edge, not for what `Vcd.wrap` records. |

The implementation clock is free: with `tick` high every third cycle the events and the JSONL are byte
identical (the strobe `sample` is the T-state, not the cycle). Pins are registered outputs of the state
(Moore), so a T-state's pins do not depend on the inputs applied during it; the harness checks this.

Port names that Vcd.wrap would drop: an input called `clock` or `reset` is filtered from the waveform (it
writes its own synthetic ones), so the RESET pin is `rst_in`.
