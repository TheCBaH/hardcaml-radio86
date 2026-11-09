#!/bin/sh
# Observe the released vm80a_sim: the MVI/STA/HLT fixtures against the K4 hand traces and the committed
# events, a manifest that names a missing path, the 8080PRE run in lockstep with Rivet's model (with
# planted model bugs that must be caught), and a byte-exact replay of an export.
# usage: observe-test.sh <release dir printed by retrocpu-fetch.sh>
set -eu
rel=$(echo "$1"/retrocpu-vm80a-sim-*)
diag=$1/retrocpu-diagnostics
here=$(cd "$(dirname "$0")/.." && pwd)
out=$here/out
manifest=$here/manifests/vm80a-fst.json
obs="opam exec -- dune exec --no-print-directory -- i8080_observe"
mkdir -p "$out"

for id in mvi-sta-hlt mvi-sta-hlt-wait2; do
    args=$(sed -e "s|@A@|$rel/fixtures|g; s|@O@|$out|g" "$rel/fixtures/$id.args")
    # shellcheck disable=SC2086
    "$rel/bin/vm80a_sim" $args +fst="$out/$id.fst" > /dev/null 2>&1
    $obs check --manifest "$manifest" --trace "$out/$id.fst"
    $obs observe --manifest "$manifest" --trace "$out/$id.fst" \
        --jsonl "$out/$id.jsonl" --text "$out/$id.txt" --perfetto "$out/$id.perfetto.json" \
        --export "$out/$id.export"
    $obs k4 --manifest "$manifest" --hand "$rel/fixtures/$id.trace" --against "$out/$id.fst"
    $obs pins --manifest "$manifest" --trace "$out/$id.fst" --pin-trace "$out/$id.trace"
    $obs replay "$out/$id.export"
    cmp "$out/$id.jsonl" "$here/test/golden/$id.jsonl"
done

# A manifest that names a path the trace does not have is refused, with the path in the message.
sed 's/"pin_wait"/"pin_waitt"/' "$manifest" > "$out/bad-manifest.json"
if $obs check --manifest "$out/bad-manifest.json" --trace "$out/mvi-sta-hlt.fst" 2> "$out/bad-manifest.err"; then
    echo "observe-test: a manifest with a missing path was accepted" >&2
    exit 1
fi
grep -q 'pin_waitt' "$out/bad-manifest.err"
echo "observe-test: the manifest naming a missing path is refused: $(head -1 "$out/bad-manifest.err")"

# 8080PRE under the K5 shim, in lockstep with the model
pre=$out/pre
mkdir -p "$pre"
python3 -I "$rel/vm80a.py" com "$diag/8080PRE.COM" "$pre" --trace -- "$rel/bin/vm80a_sim" "+fst=$pre/8080PRE.fst" > "$pre/vm.log" 2>&1
$obs pins --manifest "$manifest" --trace "$pre/8080PRE.fst" --pin-trace "$pre/8080PRE.trace" --from-pc 0x100
$obs observe --manifest "$manifest" --trace "$pre/8080PRE.fst" --export "$out/8080PRE.export"
$obs replay "$out/8080PRE.export"
$obs lockstep --known "$here/test/lockstep-known.txt" --manifest "$manifest" --trace "$pre/8080PRE.fst" \
    --program "$diag/8080PRE.COM" --shim "$diag"
# Reading the registers in T1 of the next fetch is too early on vm80a: the lockstep must notice.
sed 's/"fetch_accepted"/"fetch_t1"/' "$manifest" > "$out/t1-manifest.json"
$obs lockstep --expect-divergence --manifest "$out/t1-manifest.json" --trace "$pre/8080PRE.fst" \
    --program "$diag/8080PRE.COM" --shim "$diag"
sh "$here/scripts/retrocpu-fetch-test.sh" "$1"
for bug in aux carry; do
    $obs lockstep --plant "$bug" --expect-divergence --manifest "$manifest" --trace "$pre/8080PRE.fst" \
        --program "$diag/8080PRE.COM" --shim "$diag"
done
echo "observe-test: ok"
