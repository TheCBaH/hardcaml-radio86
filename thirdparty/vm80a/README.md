# Third-party RTL: vm80a

`vm80a` (KR580VM80A, CC BY 3.0, 1801BM1) observed through the released `vm80a_sim` (Verilator FST), used
unmodified. Nothing here builds the RTL, the simulator or Verilator: the binaries and the exerciser images
come from the [devcontainer.retrocpu-tools](https://github.com/TheCBaH/devcontainer.retrocpu-tools) release
named in the top-level `scripts/retrocpu.pin`, by tag and SHA-256 (`scripts/retrocpu-fetch.sh` refuses anything else).

| Path | What it is |
| --- | --- |
| `manifest.json` | The observation manifest of the `vm80a_sim` FST (`vm80a-fst`). |
| `golden/` | The committed events of the two fixtures; `observe-test` compares against them. |
| `scripts/` | `observe-test.sh` and `lockstep-programs.sh`. |
| `test/` | Unit tests of the manifest (no simulator needed). |

```sh
make observe-test     # fixtures vs. the K4 hand traces and the golden events, 8080PRE in lockstep, replay
make lockstep-test    # seeded programs under vm80a_sim against the model; planted model bugs must be caught
```

## Findings from vm80a (release v0.0.2)

- The pins of the waveform equal the harness's own pin trace in every sample (29, 31 and 9237 for the two
  fixtures and `8080PRE`); the harness shows the byte it will drive from `T2`, the pin carries it from the
  sample that ends the read.
- Registers lag the bus: in `T1` of the next fetch a jump has not loaded `PC` and the flags of the
  previous ALU instruction are still to come (they land in `T2`). `PC` is read as the fetch address.
- `r16_hl` is the architectural `DE` until the first `XCHG` (see `xchg` above).
- Rivet's model and the core agree on `8080PRE` and on 24 seeded programs, except the order of XTHL's two
  stack writes (`observatory/lockstep-known.txt`).
