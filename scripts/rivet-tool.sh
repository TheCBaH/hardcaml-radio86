#!/bin/sh
# Build Rivet's i8080_tool (gen: seeded programs, asm: AS-dialect assembler) from the pinned commit.
# usage: rivet-tool.sh [dest]    (default .cache/rivet); prints the path of i8080_tool.exe
set -eu
here=$(cd "$(dirname "$0")" && pwd)
. "$here/rivet.pin"
dest=${1:-.cache/rivet}
if [ ! -d "$dest/.git" ]; then
    mkdir -p "$dest"
    git -C "$dest" init -q
    git -C "$dest" remote add origin "$RIVET_REPO"
fi
if [ "$(git -C "$dest" rev-parse -q --verify HEAD 2>/dev/null || true)" != "$RIVET_COMMIT" ]; then
    git -C "$dest" fetch -q --depth 1 origin "$RIVET_COMMIT"
    git -C "$dest" checkout -q --detach FETCH_HEAD
fi
dest=$(cd "$dest" && pwd)
exe=_build/default/targets/i8080/tool/i8080_tool.exe
if [ ! -x "$dest/$exe" ]; then
    (cd "$dest" && RIVET_EXTERNAL_DEPS=true opam exec -- dune build --root . "./targets/i8080/tool/i8080_tool.exe" >&2)
fi
echo "$dest/$exe"
