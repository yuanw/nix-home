#!/usr/bin/env bash
# Populate the nix store with Workiva private git sources before build.
#
# Nix fetchgit fixed-output derivations cannot use the host SSH agent inside
# the builder (no /usr/bin/ssh on PATH). Prefetching here uses your normal
# shell environment, then nix build reuses the cached store paths.

set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
generated="$root/modules/private/_sources/generated.json"
generated_nix="$root/modules/private/_sources/generated.nix"

if [[ ! -f $generated ]]; then
  echo "prefetch-work-sources: $generated not found, skipping" >&2
  exit 0
fi

cd "$root"

# Use flake-pinned nixpkgs (not <nixpkgs>) so store paths match `nix build`.
sources_json="$(
  nix eval --impure --json --expr "
    let
      flake = builtins.getFlake \"${root}\";
      system = builtins.currentSystem;
      pkgs = flake.inputs.nixpkgs.legacyPackages.\${system};
      entries = builtins.fromJSON (builtins.readFile \"${generated}\");
      sources = pkgs.callPackage \"${generated_nix}\" {};
      names = builtins.attrNames entries;
    in pkgs.lib.genAttrs names (name: {
      url = entries.\${name}.src.url;
      rev = entries.\${name}.src.rev;
      path = builtins.unsafeDiscardStringContext (toString sources.\${name}.src);
    })
  "
)"

missing=0

while IFS=$'\t' read -r name url rev store_path; do
  [[ -z $name ]] && continue

  if nix path-info "$store_path" &>/dev/null; then
    echo "prefetch-work-sources: $name @ ${rev:0:12} (cached)"
    continue
  fi

  missing=1
  echo "prefetch-work-sources: $name @ ${rev:0:12} (fetching)"
  nix-prefetch-git "$url" --rev "$rev" >/dev/null
done < <(
  jq -r '
    to_entries[]
    | [.key, .value.url, .value.rev, .value.path]
    | @tsv
  ' <<<"$sources_json"
)

if [[ $missing -eq 0 ]]; then
  echo "prefetch-work-sources: all Workiva sources already in store"
fi
