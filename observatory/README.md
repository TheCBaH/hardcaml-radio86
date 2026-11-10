# Observatory: the RTL-independent i8080 observation

What does not depend on which 8080 is observed: it works from a manifest and a waveform, or from frames.
It names no simulator and no RTL; the third-party core ([`../thirdparty/vm80a`](../thirdparty/vm80a)) and the
locally generated one ([`../local`](../local)) each bring a manifest and use this unchanged.

| Path | What it is |
| --- | --- |
| `lib/` | `i8080_trace`: `Manifest`, `Observe`, `Discover`, `Bus`, `Instr`, `Regs`, `Trail`, `Lockstep`, `Events`, `Perfetto`, `Export` and the K4 pin-trace format (see the top-level README). |
| `bin/` | `i8080_observe`, the command over the library. |
| `docs/manifest.md` | The manifest format. |
| `lockstep-known.txt` | Differences between Rivet's model and the hardware that were investigated (XTHL's write order); `lockstep --known` takes it, anything else stops a run. |
| `testing/` | `i8080_trace_testing`, support shared by every test directory: the hand-written K4 traces, a synthetic waveform writer (`Synth`) and programs run in Rivet's model (`Model_trace`). |
| `test/` | Unit tests that need neither RTL; they use `reference.json`, the manifest of the synthetic bench. |

`make test-observatory` runs them.
