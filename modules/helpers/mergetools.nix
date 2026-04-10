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
          ];
          default = "json";
          description = "Format of the target file";
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

  # Generate content based on format
  getContent =
    task:
    if task.format == "json" then
      builtins.toJSON task.settings
    else if task.format == "yaml" then
      generators.toYAML { } task.settings
    else
      generators.toINI { } task.settings;

  # Merge into $MERGE_TMP (caller copies the real target into MERGE_TMP first so
  # symlinks are dereferenced and the result replaces the target as a writable file).
  # Uses shell variable PATCH (set in the activation script before merge).
  getMergeCmdStr =
    task:
    if task.format == "json" then
      ''${pkgs.yq-go}/bin/yq -i -o json -P --indent 2 ". *= load(\"$PATCH\")" "$MERGE_TMP"''
    else if task.format == "yaml" then
      ''${pkgs.yq-go}/bin/yq -i -oy -P ". *= load(\"$PATCH\")" "$MERGE_TMP"''
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
            "${pkgs.writeText "${name}-patch.${task.format}" (getContent task)}";
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
              cp "$PATCH" "$TARGET"
              chmod u+rw "$TARGET"
            else
              echo "mergetools: Merging Nix managed config into: $TARGET"
              MERGE_TMP="$(mktemp)"
              cat "$TARGET" > "$MERGE_TMP"
              ${getMergeCmdStr task}
              chmod u+rw "$MERGE_TMP"
              mv -f "$MERGE_TMP" "$TARGET"
            fi
          fi
        else
          echo "mergetools: Skipping merge for $TARGET (file missing & force=false)"
        fi
      ''
    ) activeTasks;
  };
}
