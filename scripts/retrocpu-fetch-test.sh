#!/bin/sh
# A tampered release asset is rejected and nothing from it is unpacked.
# usage: retrocpu-fetch-test.sh <release dir printed by retrocpu-fetch.sh>
set -eu
here=$(cd "$(dirname "$0")" && pwd)
. "$here/retrocpu.pin"
case $(uname -m) in
x86_64 | amd64) arch=x86_64 ;;
aarch64 | arm64) arch=aarch64 ;;
*) echo "retrocpu-fetch-test: unsupported machine" >&2; exit 2 ;;
esac
rel=$1
work=$(mktemp -d)
assets=$work/assets
mkdir -p "$assets"
for n in retrocpu-vm80a-sim-linux-$arch-musl retrocpu-diagnostics-$arch; do
    cp "$rel/../download/$n.tar.xz" "$assets/"
done
# flip the last byte of the vm80a archive
f=$assets/retrocpu-vm80a-sim-linux-$arch-musl.tar.xz
size=$(wc -c < "$f")
head -c $((size - 1)) "$f" > "$f.cut"
printf 'X' >> "$f.cut"
mv "$f.cut" "$f"
if RETROCPU_ASSETS=$assets "$here/retrocpu-fetch.sh" "$work/dest" > "$work/out" 2> "$work/err"; then
    echo "retrocpu-fetch-test: a tampered archive was accepted" >&2
    exit 1
fi
grep -q 'SHA-256' "$work/err"
if [ -e "$work/dest/$RETROCPU_TAG/$arch/.vm80a.unpacked" ]; then
    echo "retrocpu-fetch-test: something was unpacked from the tampered archive" >&2
    exit 1
fi
echo "retrocpu-fetch-test: a tampered vm80a archive is rejected: $(cat "$work/err")"
