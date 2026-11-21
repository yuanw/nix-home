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

    # modules/wm/omniwm.nix lets OmniWM own ~/.config/omniwm/settings.toml and
    # merges a JSON patch into it, so the patch file's name must not make yq
    # parse it as TOML, and the values OmniWM decodes as Swift Doubles must come
    # back out with their decimal point.
    echo "[mergetools-tests] toml target: JSON patch deep merged, floats kept"
    work_t="$(mktemp -d)"
    real_t="$work_t/settings.toml"
    printf 'schemaVersion = 5\n\n[borders]\nenabled = true\nwidth = 4.0\n\n[[appRules]]\nbundleId = "com.apple.Safari"\nid = "SAFARI"\nminHeight = 220.0\n\n[[workspaces]]\nid = "ONE"\nlayoutType = "dwindle"\nname = "1"\n' > "$real_t"
    chmod a-w "$real_t"
    ln -s "$real_t" "$work_t/live.toml"
    patch_t="$work_t/.settings.toml.nix-managed"
    echo '{"borders":{"width":6.0},"appRules":[{"bundleId":"com.test","id":"X","layout":"float"}]}' > "$patch_t"
    merge="$(mktemp)"
    cat "$work_t/live.toml" > "$merge"
    yq -i -p toml -o toml -P ". *= load(\"$patch_t\")" "$merge"
    chmod u+rw "$merge"
    mv -f "$merge" "$work_t/live.toml"
    [[ ! -L "$work_t/live.toml" ]]
    grep -q "^width = 6.0$" "$work_t/live.toml"
    dump="$work_t/dump.json"
    yq -p toml -o json '.' "$work_t/live.toml" > "$dump"
    jq -e '.schemaVersion == 5 and .borders.width == 6' < "$dump" >/dev/null
    # keys the patch does not name survive the rewrite, arrays of tables are
    # replaced whole
    jq -e '.workspaces[0].name == "1" and (.appRules | length) == 1 and .appRules[0].layout == "float"' < "$dump" >/dev/null

    echo "[mergetools-tests] missing toml target is initialized as toml, not as the JSON patch"
    fresh="$work_t/fresh.toml"
    yq -o toml -P "." "$patch_t" > "$fresh"
    grep -q "^width = 6.0$" "$fresh"
    yq -p toml -o json '.' "$fresh" > "$dump"
    jq -e '.appRules[0].id == "X"' < "$dump" >/dev/null

    # The guard that keeps a merge OmniWM could not read off the live file, run
    # the way the activation script runs it.
    echo "[mergetools-tests] unparseable toml target fails the merge, file untouched"
    bad="$work_t/bad.toml"
    printf 'this is not toml\n' > "$bad"
    merge="$(mktemp)"
    cat "$bad" > "$merge"
    if yq -i -p toml -o toml -P ". *= load(\"$patch_t\")" "$merge" && yq -p toml '.' "$merge" > /dev/null; then
      chmod u+rw "$merge"
      mv -f "$merge" "$bad"
    fi
    printf 'this is not toml\n' | cmp -s - "$bad"

    touch "$out"
  ''
