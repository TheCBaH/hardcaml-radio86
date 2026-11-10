#!/bin/sh
# The Hardcaml core in Cyclesim on the MVI/STA/HLT fixtures of the retrocpu release: T-states and writes,
# the pin trace against the hand-written K4 trace, the Vcd.wrap waveform decoded by hwfrp (events equal
# the K4 hand trace, equal on a faster implementation clock), export and byte-exact replay.
# usage: rtl-test.sh <release dir printed by retrocpu-fetch.sh>
set -eu
rel=$(echo "$1"/retrocpu-vm80a-sim-*)
here=$(cd "$(dirname "$0")/.." && pwd)
root=$(cd "$here/.." && pwd)
out=$root/out/rtl
manifest=$here/manifest.json
run="opam exec -- dune exec --no-print-directory -- i8080_rtl_run"
obs="opam exec -- dune exec --no-print-directory -- i8080_observe"
mkdir -p "$out"

for spec in mvi-sta-hlt:27 mvi-sta-hlt-wait2:29; do
    id=${spec%:*} want=${spec#*:}
    $run run --hex --image "$rel/fixtures/$id.run.img" --stim "$rel/fixtures/$id.stim" \
        --vcd "$out/$id.vcd" --pin-trace "$out/$id.pin.trace" > "$out/$id.run.txt"
    cat "$out/$id.run.txt"
    grep -qx "tstates $want" "$out/$id.run.txt"
    test "$(grep -c '^write ' "$out/$id.run.txt")" = 1
    grep -q '^write 2a@2000 ' "$out/$id.run.txt"
    $obs compare-pins --expected "$rel/fixtures/$id.trace" --actual "$out/$id.pin.trace"
    $obs check --manifest "$manifest" --trace "$out/$id.vcd"
    $obs observe --manifest "$manifest" --trace "$out/$id.vcd" --jsonl "$out/$id.jsonl" \
        --text "$out/$id.txt" --perfetto "$out/$id.perfetto.json" --export "$out/$id.export"
    $obs k4 --manifest "$manifest" --hand "$rel/fixtures/$id.trace" --against "$out/$id.vcd"
    $obs pins --manifest "$manifest" --trace "$out/$id.vcd" --pin-trace "$out/$id.pin.trace"
    $obs replay "$out/$id.export"
    # the executed instructions and the state they update, and ovcd's declaration list
    $obs trail --manifest "$manifest" --trace "$out/$id.vcd" > "$out/$id.trail.txt"
    grep -q '^0002 sta 0x2000 .*mem\[2000\]:?->2a' "$out/$id.trail.txt"
    grep -qx 'mem 2000: 2a' "$out/$id.trail.txt"
    $obs signals --trace "$out/$id.vcd" --like wait > "$out/$id.signals.jsonl"
    grep -q '"width":1' "$out/$id.signals.jsonl"
    # the T-state is the strobe, not the clock cycle: a clock three times faster gives the same events
    $run run --hex --image "$rel/fixtures/$id.run.img" --stim "$rel/fixtures/$id.stim" --tick-every 3 \
        --vcd "$out/$id.x3.vcd" > /dev/null
    $obs observe --manifest "$manifest" --trace "$out/$id.x3.vcd" --jsonl "$out/$id.x3.jsonl" 2> /dev/null
    cmp "$out/$id.jsonl" "$out/$id.x3.jsonl"
done

# an opcode the core does not implement stops it with a reason
printf '\000' > "$out/nop.img"
if $run run --image "$out/nop.img" --stim "$rel/fixtures/mvi-sta-hlt.stim" > "$out/nop.run.txt"; then
    echo "rtl-test: an unimplemented opcode was accepted" >&2
    exit 1
fi
grep -q 'opcode not implemented' "$out/nop.run.txt"

# a manifest that names a path the waveform lacks is refused
sed 's/"wait_"/"waitt"/' "$manifest" > "$out/bad-manifest.json"
if $obs check --manifest "$out/bad-manifest.json" --trace "$out/mvi-sta-hlt.vcd" 2> "$out/bad-manifest.err"; then
    echo "rtl-test: a manifest with a missing path was accepted" >&2
    exit 1
fi
grep -q 'waitt' "$out/bad-manifest.err"
grep -q 'hint: wait: nearest by name' "$out/bad-manifest.err"
$run verilog --output "$out/i8080_core.v"
grep -q 'module i8080_core' "$out/i8080_core.v"
echo "rtl-test: ok"
