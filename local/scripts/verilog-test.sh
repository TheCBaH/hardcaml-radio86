#!/bin/sh
# The emitted Verilog under Icarus Verilog against Cyclesim: every clock cycle of a run (the inputs the
# harness applied, the outputs Cyclesim showed) is replayed into i8080_core by the generated testbench.
# The runs are the fixtures (one on a faster implementation clock, one with a reset in the middle),
# io-console and 8080PRE.
# usage: verilog-test.sh <release dir printed by retrocpu-fetch.sh>
set -eu
rel=$(echo "$1"/retrocpu-vm80a-sim-*)
diag=$1/retrocpu-diagnostics
root=$(cd "$(dirname "$0")/../.." && pwd)
out=$root/out/verilog
run="opam exec -- dune exec --no-print-directory -- i8080_rtl_run"
mkdir -p "$out"

$run verilog --output "$out/i8080_core.v" --testbench "$out/i8080_core_tb.v"
iverilog -g2012 -Wall -o "$out/tb.vvp" "$out/i8080_core_tb.v" "$out/i8080_core.v" 2> "$out/iverilog.log"
if [ -s "$out/iverilog.log" ]; then
    cat "$out/iverilog.log" >&2
    echo "verilog-test: iverilog -Wall is not clean" >&2
    exit 1
fi

check() { # id
    vvp -n "$out/tb.vvp" "+vectors=$out/$1.vec" > "$out/$1.tb.txt"
    tail -n 12 "$out/$1.tb.txt"
    grep -q '^tb: [1-9][0-9]* cycles, 0 mismatches$' "$out/$1.tb.txt"
}

fx=$rel/fixtures
$run run --hex --image "$fx/mvi-sta-hlt.run.img" --stim "$fx/mvi-sta-hlt.stim" \
    --vectors "$out/fixture.vec" > /dev/null
check fixture
$run run --hex --image "$fx/mvi-sta-hlt-wait2.run.img" --stim "$fx/mvi-sta-hlt-wait2.stim" --tick-every 3 \
    --vectors "$out/wait2-x3.vec" > /dev/null
check wait2-x3
printf 'reset 0 @4\nreset 1 @14\nreset 0 @17\n' > "$out/reset.stim"
$run run --hex --image "$fx/mvi-sta-hlt.run.img" --stim "$out/reset.stim" --tick-every 2 \
    --vectors "$out/reset.vec" > /dev/null
check reset
$run com --hex "$fx/io-console.run.img" --stim "$fx/io-console.run.stim" --in-default 0xaa \
    --vectors "$out/io-console.vec" > /dev/null
check io-console
$run com "$diag/8080PRE.COM" --vectors "$out/8080PRE.vec" > /dev/null
check 8080PRE

# the testbench does compare: a vector with one expected bit flipped is reported
awk 'NR == 20 { $0 = substr($0, 1, length($0) - 1) (substr($0, length($0)) == "0" ? "1" : "0") } { print }' \
    "$out/fixture.vec" > "$out/planted.vec"
vvp -n "$out/tb.vvp" "+vectors=$out/planted.vec" > "$out/planted.tb.txt"
grep -q '^tb: [1-9][0-9]* cycles, 1 mismatches$' "$out/planted.tb.txt"
echo "verilog-test: ok"
