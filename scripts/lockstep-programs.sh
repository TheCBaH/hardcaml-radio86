#!/bin/sh
# Rivet's seeded programs (A10) run under the released vm80a_sim, observed from the FST and replayed
# in Rivet's behavioural model, instruction by instruction.
# usage: lockstep-programs.sh <release dir> <i8080_tool.exe> <seeds> <units> [plant]
# With [plant] (ult, aux or carry) a divergence is required in at least one program.
set -eu
rel=$1 tool=$2 seeds=$3 units=$4 plant=${5:-}
vm=$(echo "$rel"/retrocpu-vm80a-sim-*)
diag=$rel/retrocpu-diagnostics
here=$(cd "$(dirname "$0")/.." && pwd)
work=$here/out/programs
mkdir -p "$work"
obs="dune exec --no-print-directory -- i8080_observe"
caught=0 agree=0 instrs=0 classified=0
s=1
while [ "$s" -le "$seeds" ]; do
    d=$work/p$s
    mkdir -p "$d"
    "$tool" gen "$s" "$units" > "$d/prog.asm"
    "$tool" asm "$d/prog.asm" -o "$d/prog.com" > /dev/null
    python3 -I "$vm/vm80a.py" com "$d/prog.com" "$d" --trace -- "$vm/bin/vm80a_sim" "+fst=$d/prog.fst" > "$d/vm.log" 2>&1 ||
        { cat "$d/vm.log"; echo "seed $s: vm80a_sim failed" >&2; exit 1; }
    if [ -n "$plant" ]; then
        if $obs lockstep --plant "$plant" --expect-divergence --known "$here/test/lockstep-known.txt" --manifest "$here/manifests/vm80a-fst.json" \
            --trace "$d/prog.fst" --program "$d/prog.com" --shim "$diag" > "$d/lockstep.txt" 2>&1; then
            caught=$((caught + 1))
        fi
    else
        $obs lockstep --known "$here/test/lockstep-known.txt" --manifest "$here/manifests/vm80a-fst.json" --trace "$d/prog.fst" \
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
    echo "lockstep-programs: planted bug $plant caught in $caught of $seeds programs"
    [ "$caught" -gt 0 ]
else
    echo "lockstep-programs: $agree of $seeds programs agree with the model ($instrs instructions, $classified classified differences)"
fi
