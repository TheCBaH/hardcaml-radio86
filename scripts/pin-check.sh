#!/bin/sh
# The Rivet commit that thirdparty/vm80a/scripts/rivet-tool.sh builds must be the one the opam pins install.
set -eu
here=$(cd "$(dirname "$0")/.." && pwd)
. "$here/thirdparty/vm80a/scripts/rivet.pin"
for f in .devcontainer/devcontainer.json hardcaml-radio86.opam.template hardcaml-radio86.opam; do
    grep -q "rivet.git#$RIVET_COMMIT" "$here/$f" || { echo "$f does not pin Rivet $RIVET_COMMIT" >&2; exit 1; }
done
echo "pin-check: Rivet $RIVET_COMMIT"
