#!/bin/sh
# The Hardcaml core in Cyclesim on the MVI/STA/HLT fixtures of the retrocpu release: T-states and writes,
# the pin trace against the hand-written K4 trace, the Vcd.wrap waveform decoded by hwfrp (events equal
# the K4 hand trace, equal on a faster implementation clock), export and byte-exact replay. Then the
# programs that need the whole instruction set, each run on vm80a_sim too: the io-console fixture and
# 8080PRE (the console text, the OUTs, the T-states, every bus transfer and instruction equal to vm80a's,
# lockstep with Rivet's model).
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

# the same program on vm80a_sim and on the Hardcaml core: equal OUTs, T-states, transfers and instructions
vm=$rel/bin/vm80a_sim
same() { # id program-and-options-for-com  vm80a-trace
    id=$1 vmtrace=$2
    $obs k4 --manifest "$manifest" --hand "$vmtrace" --against "$out/$id.vcd" --from-pc 0x100
}
id=io-console
args=$(sed -e "s|@A@|$rel/fixtures|g; s|@O@|$out/vm|g" "$rel/fixtures/$id.args")
mkdir -p "$out/vm"
# shellcheck disable=SC2086
"$vm" $args > /dev/null 2>&1
$run com --hex "$rel/fixtures/$id.run.img" --stim "$rel/fixtures/$id.run.stim" --in-default 0xaa \
    --vcd "$out/$id.vcd" --io-out "$out/$id.ioout" > "$out/$id.com.txt"
cmp "$out/vm/$id.ioout" "$out/$id.ioout"
want=$(sed -n 's/^tstates=//p' "$out/vm/$id.result")
grep -q "tstates $want," "$out/$id.com.txt"
same $id "$out/vm/$id.trace"

diag=$1/retrocpu-diagnostics
mkdir -p "$out/pre"
python3 -I "$rel/vm80a.py" com "$diag/8080PRE.COM" "$out/pre" --trace -- "$vm" > "$out/pre/vm.log" 2>&1
id=8080PRE
$run com "$diag/8080PRE.COM" --vcd "$out/$id.vcd" --io-out "$out/$id.ioout" > "$out/$id.com.txt"
grep -q '^8080 Preliminary tests complete' "$out/$id.com.txt"
cmp "$out/pre/$id.ioout" "$out/$id.ioout"
same $id "$out/pre/$id.trace"
$obs lockstep --known "$here/../observatory/lockstep-known.txt" --manifest "$manifest" --trace "$out/$id.vcd" \
    --program "$diag/8080PRE.COM" --shim "$diag"
echo "rtl-test: ok"
