{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.mergetools;

  homeDir = config.home.homeDirectory;

  # Strip home prefix for relative path calculation
  stripHomePrefix =
    target:
    let
      prefix = homeDir + "/";
    in
    if hasPrefix prefix target then
      removePrefix prefix target
    else
      abort "mergetools: target '${target}' must start with '${prefix}'";

  # Task data structure
  mergeTargetType = types.submodule (
    { ... }:
    {
      options = {
        enable = mkEnableOption "this merge task" // {
          default = true;
        };
        target = mkOption {
          type = types.str;
          description = "Absolute path to the target file to merge into";
        };
        settings = mkOption {
          type = types.attrs;
          default = { };
          description = "Attributes to merge into the target file";
        };
        format = mkOption {
          type = types.enum [
            "yaml"
            "json"
            "ini"
            "toml"
          ];
          default = "json";
          description = ''
            Format of the target file. A "toml" target is merged through yq, and
            its patch stays JSON (see getContent).
          '';
        };
        force = mkOption {
          type = types.bool;
          default = false;
          description = "Create the target file if it doesn't exist";
        };
        backup = mkOption {
          type = types.bool;
          default = true;
          description = "Create a backup before merging";
        };
        generateReference = mkOption {
          type = types.bool;
          default = true;
          description = "Whether to generate a .nix-managed reference file next to the target.";
        };
      };
    }
  );

  activeTasks = filterAttrs (_n: v: v.enable) cfg;

  # Generate content based on format.
  #
  # A TOML target gets a JSON patch: yq picks the parser for load(PATCH) from the
  # patch file name, and the ".nix-managed" name given below has no extension it
  # recognises, so it falls back to YAML -- a superset of JSON and not of TOML.
  # Only the target is read and written as TOML (see getMergeCmdStr), and the
  # .nix-managed reference file for a TOML target is JSON for the same reason.
  getContent =
    task:
    if task.format == "json" || task.format == "toml" then
      builtins.toJSON task.settings
    else if task.format == "yaml" then
      generators.toYAML { } task.settings
    else
      generators.toINI { } task.settings;

  # Format the patch content above is actually in, which is what names the
  # writeText fallback below -- yq sniffs that name too.
  patchFormat = task: if task.format == "toml" then "json" else task.format;

  # Content for a target that does not exist yet: the patch verbatim, except for
  # a TOML target, whose patch is JSON. No -p json there: yq's JSON parser turns
  # 4.0 into 4 and OmniWM's decoder wants the float, while its default YAML
  # parser (a superset of JSON) keeps the type.
  getInitCmdStr =
    task:
    if task.format == "toml" then
      ''${pkgs.yq-go}/bin/yq -o toml -P "." "$PATCH" > "$TARGET"''
    else
      ''cp "$PATCH" "$TARGET"'';

  # Merge into $MERGE_TMP (caller copies the real target into MERGE_TMP first so
  # symlinks are dereferenced and the result replaces the target as a writable file).
  # Uses shell variable PATCH (set in the activation script before merge).
  getMergeCmdStr =
    task:
    if task.format == "json" then
      ''${pkgs.yq-go}/bin/yq -i -o json -P --indent 2 ". *= load(\"$PATCH\")" "$MERGE_TMP"''
    else if task.format == "yaml" then
      ''${pkgs.yq-go}/bin/yq -i -oy -P ". *= load(\"$PATCH\")" "$MERGE_TMP"''
    else if task.format == "toml" then
      # The re-parse is the guard, not decoration: a writer that produced
      # something unreadable would otherwise land on a target OmniWM validates as
      # a unit and falls back to defaults on (settings.toml.corrupt).
      ''(${pkgs.yq-go}/bin/yq -i -p toml -o toml -P ". *= load(\"$PATCH\")" "$MERGE_TMP" && ${pkgs.yq-go}/bin/yq -p toml -o json "." "$MERGE_TMP" > /dev/null)''
    else
      ''${pkgs.crudini}/bin/crudini --merge "$MERGE_TMP" < "$PATCH"'';
in
{
  options.mergetools = mkOption {
    type = types.attrsOf mergeTargetType;
    default = { };
    description = "Declarative config merging with backup support (Home Manager Only)";
  };

  config = mkIf (activeTasks != { }) {
    # 1. Generate reference files (home.file)
    home.file =
      let
        tasksWithRef = filterAttrs (_n: v: v.generateReference) activeTasks;
      in
      mapAttrs' (
        _name: task:
        let
          relTarget = stripHomePrefix task.target;
          dir = dirOf relTarget;
          base = baseNameOf relTarget;
          refPath = if dir == "." then ".${base}.nix-managed" else "${dir}/.${base}.nix-managed";
        in
        nameValuePair refPath { text = getContent task; }
      ) tasksWithRef;

    # 2. Inject activation scripts with backup support
    home.activation = mapAttrs (
      name: task:
      let
        relTarget = stripHomePrefix task.target;
        dir = dirOf relTarget;
        base = baseNameOf relTarget;
        patchPath =
          if task.generateReference then
            if dir == "." then "$HOME/.${base}.nix-managed" else "$HOME/${dir}/.${base}.nix-managed"
          else
            "${pkgs.writeText "${name}-patch.${patchFormat task}" (getContent task)}";
      in
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        TARGET="${task.target}"
        PATCH="${patchPath}"
        FORCE="${if task.force then "true" else "false"}"
        BACKUP="${if task.backup then "true" else "false"}"

        if [ -f "$TARGET" ] || [ "$FORCE" = "true" ]; then
          if [ -f "$PATCH" ]; then
            mkdir -p "$(dirname "$TARGET")"

            # Create backup if enabled and target exists (cat follows symlinks)
            if [ "$BACKUP" = "true" ] && [ -f "$TARGET" ]; then
              BACKUP_FILE="''${TARGET}.backup.$(date +%Y%m%d%H%M%S)"
              echo "mergetools: Creating backup: $BACKUP_FILE"
              cat "$TARGET" > "$BACKUP_FILE"
            fi

            if [ ! -f "$TARGET" ]; then
              echo "mergetools: Initializing missing config from patch: $TARGET"
              ${getInitCmdStr task}
              chmod u+rw "$TARGET"
            else
              echo "mergetools: Merging Nix managed config into: $TARGET"
              MERGE_TMP="$(mktemp)"
              cat "$TARGET" > "$MERGE_TMP"
              # Never mv a merge that failed: MERGE_TMP still holds the old
              # content, so writing it would pass a broken merge off as done.
              if ${getMergeCmdStr task}; then
                chmod u+rw "$MERGE_TMP"
                mv -f "$MERGE_TMP" "$TARGET"
              else
                echo "mergetools: merge failed, leaving $TARGET alone"
                rm -f "$MERGE_TMP"
              fi
            fi
          fi
        else
          echo "mergetools: Skipping merge for $TARGET (file missing & force=false)"
        fi
      ''
    ) activeTasks;
  };
}
