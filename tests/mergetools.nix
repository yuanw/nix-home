{
  pkgs,
  lib,
}:

pkgs.runCommand "mergetools-tests"
  {
    nativeBuildInputs = [
      pkgs.jq
      pkgs.yq-go
    ];
    meta = {
      description = "Regression tests for modules/helpers/mergetools.nix JSON merge pipeline";
    };
  }
  ''
    set -euo pipefail
    yq() { ${lib.getExe pkgs.yq-go} "$@"; }

    echo "[mergetools-tests] deep merge (yq *=), extension-settings style"
    base="$(mktemp)"
    patch="$(mktemp)"
    merge="$(mktemp)"
    echo '{"version":2,"commands":{"keep":{"n":1}}}' > "$base"
    echo '{"version":3,"commands":{"fromnix":{"precedenceList":[]}}}' > "$patch"
    cat "$base" > "$merge"
    yq -i -o json -P --indent 2 ". *= load(\"$patch\")" "$merge"
    [[ "$(jq -r .version < "$merge")" == 3 ]]
    jq -e '.commands.keep' < "$merge" >/dev/null
    jq -e '.commands.fromnix' < "$merge" >/dev/null

    echo "[mergetools-tests] symlink target materialized to writable regular file"
    work="$(mktemp -d)"
    real="$work/real.json"
    echo '{"version":1,"commands":{"a":1}}' > "$real"
    chmod a-w "$real"
    ln -s "$real" "$work/target.json"
    merge="$(mktemp)"
    cat "$work/target.json" > "$merge"
    echo '{"commands":{"b":{"k":true}}}' > "$patch"
    yq -i -o json -P --indent 2 ". *= load(\"$patch\")" "$merge"
    chmod u+rw "$merge"
    mv -f "$merge" "$work/target.json"
    [[ -f "$work/target.json" ]]
    [[ ! -L "$work/target.json" ]]
    jq -e '.commands.a and .commands.b' < "$work/target.json" >/dev/null

    echo "[mergetools-tests] cat for backup follows symlink (content, not link)"
    backup="$(mktemp)"
    ln -s "$work/target.json" "$work/via-link.json"
    cat "$work/via-link.json" > "$backup"
    jq -e '.commands.b.k == true' < "$backup" >/dev/null

    touch "$out"
  ''
