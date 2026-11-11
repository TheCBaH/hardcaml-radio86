#!/bin/sh
# The math workloads (workloads/) on the released vm80a_sim: each fixture's image runs from reset to HLT
# with no shim and no boot preamble, and the 64 KiB it leaves and its T-state count must equal what
# Rivet's model leaves and counts (the expected RAM itself is checked against the OCaml oracles by the unit
# tests, on the model and on the Hardcaml core).
# usage: workloads.sh <release dir printed by retrocpu-fetch.sh>
set -eu
rel=$1
vm=$(echo "$rel"/retrocpu-vm80a-sim-*)/bin/vm80a_sim
here=$(cd "$(dirname "$0")/.." && pwd)
root=$(cd "$here/../.." && pwd)
out=$root/out/workloads
mkdir -p "$out"
opam exec -- dune exec --no-print-directory -- ./workloads/bin/workload_files.exe "$out"
for img in "$out"/*.img; do
    id=$(basename "$img" .img)
    "$vm" "+image=$img" +origin=0 +fill=0 +ram_start=0 +ram_size=10000 "+max_tstates=$(cat "$out/$id.budget")" \
        "+result=$out/$id.result" "+membin=$out/$id.mem" "+io_out=$out/$id.ioout" > /dev/null 2>&1
    grep -qx 'stop_reason=hlt' "$out/$id.result" || { cat "$out/$id.result"; echo "$id: vm80a_sim did not halt" >&2; exit 1; }
    want=$(cat "$out/$id.tstates")
    grep -qx "tstates=$want" "$out/$id.result" || { cat "$out/$id.result"; echo "$id: expected $want T-states" >&2; exit 1; }
    cmp "$out/$id.mem" "$out/$id.final" || { echo "$id: vm80a_sim memory differs from the model's" >&2; exit 1; }
    test ! -s "$out/$id.ioout" || { echo "$id: port traffic" >&2; exit 1; }
    echo "workloads: $id: vm80a_sim and the model agree ($want T-states, 64 KiB equal, no port traffic)"
done
