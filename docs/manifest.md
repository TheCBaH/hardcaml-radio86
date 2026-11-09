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

`xchg` is for a core that keeps which physical pair is `DE` and which is `HL` (vm80a does: `XCHG`
flips a bit and moves no data). A manifest without it says `DE` and `HL` are the architectural pairs.

## Adding a CPU

The Hardcaml CPU (goal P) writes its Cyclesim trace with `Vcd.wrap` and a one-cycle sample strobe per
T-state, and gets its own manifest: the same 13 pin names, its register paths, `clock.enable` set to the
strobe. `Observe`, `Bus`, `Instr` and `Lockstep` do not change.

## Findings from vm80a (release v0.0.2)

- The pins of the waveform equal the harness's own pin trace in every sample (29, 31 and 9237 for the two
  fixtures and `8080PRE`); the harness shows the byte it will drive from `T2`, the pin carries it from the
  sample that ends the read.
- Registers lag the bus: in `T1` of the next fetch a jump has not loaded `PC` and the flags of the
  previous ALU instruction are still to come (they land in `T2`). `PC` is read as the fetch address.
- `r16_hl` is the architectural `DE` until the first `XCHG` (see `xchg` above).
- Rivet's model and the core agree on `8080PRE` and on 24 seeded programs, except the order of XTHL's two
  stack writes (`test/lockstep-known.txt`).
