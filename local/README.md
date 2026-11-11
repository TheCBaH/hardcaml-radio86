# Locally generated RTL: the Hardcaml i8080

The CPU written in this repository, in Hardcaml (v0.17.1, OCaml 5.4.1), simulated in Cyclesim and observed
with the common observatory like any other core.

| Path | What it is |
| --- | --- |
| `rtl/` | `i8080_rtl`: `Plan` (the machine cycles of each opcode), `Core` (datapath and sequencer, all 256 opcodes), `Stimulus` (the K4 schedule, `io` and `inta` included), `Harness` (Cyclesim around the core with 64 KiB of RAM, ports and the K5 shim). |
| `trace/` | `i8080_rtl_trace`: the harness's samples as the observatory's frames (the pin trace of the run). |
| `bin/` | `i8080_rtl_run`: `run` a program, `com` a CP/M `.COM` under the K5 shim, `verilog`; writes the `Vcd.wrap` waveform and the K4 pin trace. |
| `manifest.json` | The observation manifest of the `Vcd.wrap` waveform (`hardcaml-vcd`). |
| `scripts/` | `rtl-test.sh` (the release's fixtures, `io-console` and `8080PRE` on the core and on vm80a_sim), `verilog-test.sh` (the emitted Verilog under Icarus Verilog, every clock cycle against Cyclesim) and `lockstep-programs.sh` (seeded programs against Rivet's model). |
| `test/` | Unit tests: the cycle plan against Rivet's table, all 256 opcodes in lockstep with Rivet's model, the fixtures, the waveform and its observation. |

```sh
make test-local   # the unit tests
make rtl-test     # fixtures, 8080PRE and io-console against vm80a_sim, a faster clock, export and replay
make verilog-test # the emitted Verilog replays Cyclesim's runs: same outputs in every clock cycle
make rtl-lockstep-test
```

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
