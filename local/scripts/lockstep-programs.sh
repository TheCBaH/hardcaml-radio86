#!/bin/sh
# Rivet's seeded programs (A10) run on the Hardcaml core and replayed in Rivet's behavioural model,
# instruction by instruction. Same programs, shim and known-difference list as the vm80a run.
# usage: lockstep-programs.sh <release dir> <i8080_tool.exe> <seeds> <units> [plant]
# With [plant] (ult, aux or carry) a divergence is required in at least one program.
set -eu
rel=$1 tool=$2 seeds=$3 units=$4 plant=${5:-}
diag=$rel/retrocpu-diagnostics
here=$(cd "$(dirname "$0")/.." && pwd)
root=$(cd "$here/.." && pwd)
work=$root/out/rtl-programs
mkdir -p "$work"
obs="opam exec -- dune exec --no-print-directory -- i8080_observe"
run="opam exec -- dune exec --no-print-directory -- i8080_rtl_run"
known=$root/observatory/lockstep-known.txt
caught=0 agree=0 instrs=0 classified=0
s=1
while [ "$s" -le "$seeds" ]; do
    d=$work/p$s
    mkdir -p "$d"
    "$tool" gen "$s" "$units" > "$d/prog.asm"
    "$tool" asm "$d/prog.asm" -o "$d/prog.com" > /dev/null
    $run com "$d/prog.com" --vcd "$d/prog.vcd" > "$d/run.txt" 2>&1 ||
        { cat "$d/run.txt"; echo "seed $s: the core did not stop at OUT FF" >&2; exit 1; }
    if [ -n "$plant" ]; then
        if $obs lockstep --plant "$plant" --expect-divergence --known "$known" --manifest "$here/manifest.json" \
            --trace "$d/prog.vcd" --program "$d/prog.com" --shim "$diag" > "$d/lockstep.txt" 2>&1; then
            caught=$((caught + 1))
        fi
    else
        $obs lockstep --known "$known" --manifest "$here/manifest.json" --trace "$d/prog.vcd" \
            --program "$d/prog.com" --shim "$diag" > "$d/lockstep.txt" 2>&1 ||
            { cat "$d/lockstep.txt"; echo "seed $s: lockstep diverged" >&2; exit 1; }
        n=$(sed -n 's/^\([0-9]*\) instructions agree.*/\1/p' "$d/lockstep.txt")
        instrs=$((instrs + n))
        c=$(sed -n 's/^  classified: \([0-9]*\) x.*/\1/p' "$d/lockstep.txt" | head -1)
        classified=$((classified + ${c:-0}))
        agree=$((agree + 1))
    fi
    s=$((s + 1))
done
if [ -n "$plant" ]; then
    echo "rtl-lockstep-programs: planted bug $plant caught in $caught of $seeds programs"
    [ "$caught" -gt 0 ]
else
    echo "rtl-lockstep-programs: $agree of $seeds programs agree with the model ($instrs instructions, $classified classified differences)"
fi
