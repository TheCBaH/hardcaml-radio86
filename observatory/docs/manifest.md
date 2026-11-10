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

What each RTL's manifest claims is in its own directory: [local/README.md](../../local/README.md) (the Hardcaml
core, `local/manifest.json`) and [thirdparty/vm80a/README.md](../../thirdparty/vm80a/README.md)
(`thirdparty/vm80a/manifest.json`).
