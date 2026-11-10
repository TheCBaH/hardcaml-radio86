#!/bin/sh
# Fetch the pinned retrocpu-tools release, verify it, unpack it for this arch.
# usage: retrocpu-fetch.sh [dest]        (default .retrocpu)
#   RETROCPU_ASSETS=<dir>  take the release assets from a local directory
#                          instead of downloading (still verified)
# Prints the unpack directory on success. Fails if any hash differs from
# scripts/retrocpu.pin; nothing is unpacked from an unverified file.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
. "$here/retrocpu.pin"
dest=${1:-.retrocpu}
case $(uname -m) in
x86_64 | amd64) arch=x86_64 ;;
aarch64 | arm64) arch=aarch64 ;;
*) echo "retrocpu-fetch: unsupported machine $(uname -m)" >&2; exit 2 ;;
esac
mkdir -p "$dest"; dest=$(cd "$dest" && pwd)
out=$dest/$RETROCPU_TAG/$arch
dl=$dest/$RETROCPU_TAG/download
mkdir -p "$dl" "$out"
assets="diagnostics:retrocpu-diagnostics-$arch vm80a:retrocpu-vm80a-sim-linux-$arch-musl"
for a in $assets; do
    key=${a%%:*} name=${a#*:}.tar.xz
    want=$(eval echo "\$RETROCPU_SHA256_${key}_$arch")
    f=$dl/$name
    if [ -n "${RETROCPU_ASSETS:-}" ]; then
        cp "$RETROCPU_ASSETS/$name" "$f"
    elif [ ! -f "$f" ]; then
        # The releases/download URL answers 504 for some assets for long periods; the API asset URL
        # does not. Either way the SHA-256 below decides.
        if ! curl -fsSL --retry 3 --retry-all-errors --retry-delay 5 -o "$f" \
            "https://github.com/$RETROCPU_REPO/releases/download/$RETROCPU_TAG/$name"; then
            echo "retrocpu-fetch: $name: download URL failed, using the API asset URL" >&2
            api=https://api.github.com/repos/$RETROCPU_REPO/releases/tags/$RETROCPU_TAG
            auth=${GITHUB_TOKEN:+"Authorization: Bearer ${GITHUB_TOKEN:-}"}
            url=$(curl -fsSL --retry 3 --retry-all-errors ${auth:+-H "$auth"} "$api" |
                python3 -I -c 'import json,sys
for a in json.load(sys.stdin)["assets"]:
    if a["name"] == sys.argv[1]:
        print(a["url"])' "$name")
            curl -fsSL --retry 6 --retry-all-errors --retry-delay 10 ${auth:+-H "$auth"} \
                -H 'Accept: application/octet-stream' -o "$f" "$url"
        fi
    fi
    got=$(sha256sum "$f" | cut -d' ' -f1)
    if [ "$got" != "$want" ]; then
        mv "$f" "$f.rejected"
        echo "retrocpu-fetch: $name: SHA-256 $got, pinned $want" >&2
        exit 1
    fi
    if [ ! -f "$out/.$key.unpacked" ]; then
        tar -xJf "$f" -C "$out"
        : > "$out/.$key.unpacked"
    fi
done
echo "$out"
