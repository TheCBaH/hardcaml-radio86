#!/bin/sh
# Run the released vm80a_sim on the MVI/STA/HLT fixtures, observe their FSTs and compare
# with the hand-written K4 traces and the committed events.
# usage: observe-test.sh <release dir printed by retrocpu-fetch.sh>
set -eu
rel=$1/retrocpu-vm80a-sim-*
rel=$(echo $rel)
here=$(cd "$(dirname "$0")/.." && pwd)
out=$here/out
obs="dune exec --no-print-directory -- i8080_observe"
mkdir -p "$out"
fixtures="mvi-sta-hlt mvi-sta-hlt-wait2"
for id in $fixtures; do
    args=$(sed -e "s|@A@|$rel/fixtures|g; s|@O@|$out|g" "$rel/fixtures/$id.args")
    # shellcheck disable=SC2086
    "$rel/bin/vm80a_sim" $args +fst="$out/$id.fst" >/dev/null 2>&1
    $obs check --manifest "$here/manifests/vm80a-fst.json" --trace "$out/$id.fst"
    $obs observe --manifest "$here/manifests/vm80a-fst.json" --trace "$out/$id.fst" \
        --jsonl "$out/$id.jsonl" --text "$out/$id.txt" --perfetto "$out/$id.perfetto.json" \
        --export "$out/$id.export"
    $obs k4 --manifest "$here/manifests/vm80a-fst.json" --hand "$rel/fixtures/$id.trace" --against "$out/$id.fst"
    $obs replay "$out/$id.export"
    cmp "$out/$id.jsonl" "$here/test/golden/$id.jsonl"
done
echo "observe-test: ok"
