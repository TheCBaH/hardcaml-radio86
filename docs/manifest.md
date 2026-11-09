# The observation manifest (`i8080-observation-manifest/1`)

A manifest says where the pins and registers of an 8080 are in a waveform and what one T-state is. It
is the claim about the hardware: nothing is probed, and a path that is not in the trace, a width that
differs, or a name outside the profile is an error that lists every such probe.

```json
{ "format": "i8080-observation-manifest/1", "id": "vm80a-fst", "profile": "i8080a/1",
  "clock": { "path": ["i8080_tb", "clk"], "rule": "after_rising", "enable": { "path": ["i8080_tb", "f2"] } },
  "snapshot": "fetch_accepted",
  "signals": [ { "name": "a", "path": ["i8080_tb", "pin_a"], "width": 16, "kind": "pin" }, … ] }
```

| Member | Meaning |
| --- | --- |
| `clock.rule` | hwfrp's sample rule: `after_rising`, `after_falling` or `before_rising`. |
| `clock.enable` | A 1-bit signal; only samples where it is high are T-states. vm80a: `f2`, so a T-state is the second `pin_clk` rising edge (K2). A CPU with a faster implementation clock names its sample strobe. |
| `signals[].kind` | `pin`: the 13 pins of the profile, all required (`a do di sync dbin wr_n wait hlda inte ready hold int reset`). `register`: internal flops, all or none of `acc bc de hl sp pc z s p c ac`, plus the optional `wz` and `xchg`. |
| `snapshot` | Where the registers after an instruction are read. `fetch_accepted`: in the sample that accepts the next opcode (T3 without waits). `fetch_t1` is the next fetch's first T-state, which vm80a has not finished writing back by then. |
| `trace` | Only in an exported manifest: file, format, SHA-256 and size of the trace the events came from. |

`signals[].valid` (optional): a 1-bit signal that says when this one is driven. A Hardcaml model has no
tri-state, so its `do` pin always has a value; with `valid` set to `dout_en` the frame says unknown where the
core drives nothing, as the vm80a trace does with `z`.

`xchg` is for a core that keeps which physical pair is `DE` and which is `HL` (vm80a does: `XCHG`
flips a bit and moves no data). A manifest without it says `DE` and `HL` are the architectural pairs.

## The Hardcaml core (`manifests/hardcaml-vcd.json`)

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

## Findings from vm80a (release v0.0.2)

- The pins of the waveform equal the harness's own pin trace in every sample (29, 31 and 9237 for the two
  fixtures and `8080PRE`); the harness shows the byte it will drive from `T2`, the pin carries it from the
  sample that ends the read.
- Registers lag the bus: in `T1` of the next fetch a jump has not loaded `PC` and the flags of the
  previous ALU instruction are still to come (they land in `T2`). `PC` is read as the fetch address.
- `r16_hl` is the architectural `DE` until the first `XCHG` (see `xchg` above).
- Rivet's model and the core agree on `8080PRE` and on 24 seeded programs, except the order of XTHL's two
  stack writes (`test/lockstep-known.txt`).
