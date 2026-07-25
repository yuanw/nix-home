#!/usr/bin/env bash
# Bump Workiva git sources whose tags need semver ordering (nvfetcher use_max_tag is lexical).
# parsimony is excluded: pinned to PR branch in nvfetcher.toml.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
generated_json="$root/modules/private/_sources/generated.json"
render_nix="$root/scripts/render-work-generated-nix.py"

declare -A packages=(
  [wk]="ssh://git@github.com/Workiva/wk.git|^v[0-9]+\\.[0-9]+\\.[0-9]+$"
  [frugal]="ssh://git@github.com/Workiva/frugal.git|^v[0-9]+\\.[0-9]+\\.[0-9]+$"
)

latest_tag() {
  local url=$1 regex=$2
  git ls-remote --tags --refs "$url" |
    sed 's/.*refs\/tags\///' |
    grep -E "$regex" |
    sort -V |
    tail -1
}

tag_rev() {
  local url=$1 tag=$2 rev
  rev="$(git ls-remote "$url" "refs/tags/${tag}^{}" | awk 'NR==1 { print $1 }')"
  if [[ -z $rev ]]; then
    rev="$(git ls-remote "$url" "refs/tags/${tag}" | awk 'NR==1 { print $1 }')"
  fi
  [[ -n $rev ]] || {
    echo "bump-semver-git-sources: no commit for tag $tag" >&2
    exit 1
  }
  printf '%s' "$rev"
}

commit_date() {
  local url=$1 rev=$2
  local tmp
  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' RETURN
  git -C "$tmp" init -q
  git -C "$tmp" remote add origin "$url"
  git -C "$tmp" fetch -q --depth 1 origin "$rev"
  git -C "$tmp" log -1 --format=%cs
}

prefetch_sri() {
  local url=$1 rev=$2
  nix-prefetch-git "$url" --rev "$rev" 2>/dev/null | jq -r .sha256
}

changed=0

for name in "${!packages[@]}"; do
  IFS='|' read -r url regex <<<"${packages[$name]}"
  tag="$(latest_tag "$url" "$regex")"
  rev="$(tag_rev "$url" "$tag")"
  current_rev="$(jq -r --arg n "$name" '.[$n].src.rev' "$generated_json")"
  current_tag="$(jq -r --arg n "$name" '.[$n].passthru.tag // empty' "$generated_json")"

  if [[ $rev == "$current_rev" && $tag == "$current_tag" ]]; then
    echo "bump-semver-git-sources: $name already at $tag ($rev)"
    continue
  fi

  echo "bump-semver-git-sources: $name ${current_tag:-$current_rev} -> $tag ($rev)"

  if [[ $rev == "$current_rev" ]]; then
    sha256="$(jq -r --arg n "$name" '.[$n].src.sha256' "$generated_json")"
    date="$(jq -r --arg n "$name" '.[$n].date' "$generated_json")"
  else
    sha256="$(prefetch_sri "$url" "$rev")"
    date="$(commit_date "$url" "$rev")"
  fi

  tmp="$(mktemp)"
  jq \
    --arg name "$name" \
    --arg rev "$rev" \
    --arg sha256 "$sha256" \
    --arg date "$date" \
    --arg tag "$tag" \
    '
      .[$name].src.rev = $rev
      | .[$name].src.sha256 = $sha256
      | .[$name].version = $rev
      | .[$name].date = $date
      | .[$name].passthru = ((.[$name].passthru // {}) + { tag: $tag })
    ' \
    "$generated_json" >"$tmp"
  mv "$tmp" "$generated_json"
  changed=1
done

if [[ $changed -eq 1 ]]; then
  python3 "$render_nix" "$generated_json" "$root/modules/private/_sources/generated.nix"
fi
