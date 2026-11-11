#!/bin/sh
# 8080EX1 (the instruction exerciser: 25 groups, each a CRC over the results and flags of a family of
# instructions) on the emitted Verilog under Verilator, with the generated bench: the K5 shim, the
# console on port 01, the stop at OUT FF. 8080PRE first, whose T-state count Cyclesim gives too.
# 23,803,352,043 T-states: the count of Rivet's model and of vm80a_sim for the same run. About half an
# hour of one core, which is why this is not part of `make ci`.
# usage: exerciser-test.sh <release dir printed by retrocpu-fetch.sh>
set -eu
diag=$1/retrocpu-diagnostics
root=$(cd "$(dirname "$0")/../.." && pwd)
out=$root/out/exerciser
run="opam exec -- dune exec --no-print-directory -- i8080_rtl_run"
mkdir -p "$out"

$run verilog --output "$out/i8080_core.v" --bench "$out/i8080_bench.v"
verilator --cc --exe --build -j 0 -O3 --x-assign fast --x-initial fast --noassert -Wno-fatal \
    +define+EXTERNAL_CLOCK --top-module i8080_bench -CFLAGS -O3 --Mdir "$out/obj" -o i8080_bench \
    "$out/i8080_bench.v" "$out/i8080_core.v" "$root/local/verilog/bench_main.cpp" > "$out/verilator.log" 2>&1 ||
    { cat "$out/verilator.log" >&2; exit 1; }

# the programs end their lines with LF CR
"$out/obj/i8080_bench" "+com=$diag/8080PRE.COM" | tr -d '\r' > "$out/8080PRE.txt"
grep -q '^8080 Preliminary tests complete' "$out/8080PRE.txt"
want=$($run com "$diag/8080PRE.COM" | sed -n 's/.*tstates \([0-9]*\),.*/\1/p')
grep -qx "stop port ff, tstates $want" "$out/8080PRE.txt"

"$out/obj/i8080_bench" "+com=$diag/8080EX1.COM" | tr -d '\r' > "$out/8080EX1.txt"
cat "$out/8080EX1.txt"
test "$(grep -c ' OK' "$out/8080EX1.txt")" = 25
if grep -q 'ERROR' "$out/8080EX1.txt"; then exit 1; fi
grep -q '^Tests complete' "$out/8080EX1.txt"
grep -qx 'stop port ff, tstates 23803352043' "$out/8080EX1.txt"
echo "exerciser-test: ok"
