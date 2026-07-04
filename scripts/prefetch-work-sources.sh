#!/usr/bin/env bash
# Populate the nix store with Workiva private git sources before build.
#
# Nix fetchgit fixed-output derivations cannot use the host SSH agent inside
# the builder (no /usr/bin/ssh on PATH). Prefetching here uses your normal
# shell environment, then nix build reuses the cached store paths.

set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
generated="$root/modules/private/_sources/generated.json"

if [[ ! -f $generated ]]; then
  echo "prefetch-work-sources: $generated not found, skipping" >&2
  exit 0
fi

prefetch_git() {
  local name="$1" url="$2" rev="$3" root_dir="${4:-}"

  echo "prefetch-work-sources: $name @ ${rev:0:12}"
  if [[ -n $root_dir ]]; then
    nix-prefetch-git "$url" --rev "$rev" --root-dir "$root_dir" >/dev/null
  else
    nix-prefetch-git "$url" --rev "$rev" >/dev/null
  fi
}

while IFS=$'\t' read -r name url rev; do
  [[ -z $name ]] && continue
  prefetch_git "$name" "$url" "$rev"
done < <(
  jq -r '
    to_entries[]
    | [.key, .value.src.url, .value.src.rev]
    | @tsv
  ' "$generated"
)

# Hard-coded in modules/private/work.nix (not nvfetcher-managed).
prefetch_git \
  mcp-atlassian \
  ssh://git@github.com/Workiva/mcp_tools.git \
  d5ddb35a22a3319d0e68468e0eb5686bc7d3ef3e \
  servers/atlassian
