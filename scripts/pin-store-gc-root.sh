#!/usr/bin/env bash
# Pin a store path under ~/.local/state/nix/gcroots/work-sources so GC keeps it.
set -euo pipefail

if (($# != 2)); then
  echo "usage: pin-store-gc-root.sh NAME STORE_PATH" >&2
  exit 2
fi

name=$1
store_path=$2
gcroot_dir="${XDG_STATE_HOME:-$HOME/.local/state}/nix/gcroots/work-sources"

mkdir -p "$gcroot_dir"
nix-store --add-root "$gcroot_dir/$name" --realise "$store_path" >/dev/null
