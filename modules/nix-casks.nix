# Darwin module for installing macOS .app bundles from nix-casks flake via ditto.
# Uses marker-based caching so apps are only re-copied when their store path changes.
# Apps land in /Applications/Nix Casks and are discoverable by Spotlight/Raycast.
#
# Usage:
#   environment.casks = with inputs'.nix-casks.packages; [ firefox raycast ];
{
  config,
  lib,
  ...
}:
let
  cfg = config.environment;

  syncScript = lib.concatMapStringsSep "\n" (pkg: ''
    sync_app "${pkg}"
  '') cfg.casks;
in
{
  options.environment.casks = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [ ];
    description = ''
      List of macOS application packages to install via ditto.
      Uses marker-based caching to only copy when the store path changes.
      Apps are installed to /Applications/Nix Casks.
      Packages can be taken from inputs'.nix-casks.packages.
    '';
  };

  config = lib.mkIf (cfg.casks != [ ]) {
    system.activationScripts.postActivation.text = lib.mkAfter ''
      echo "setting up /Applications/Nix Casks..." >&2
      targetDir='/Applications/Nix Casks'
      markerDir="$targetDir/.sources"
      mkdir -p "$targetDir" "$markerDir"

      declare -A wantedApps

      sync_app() {
        local src="$1"

        for app in "$src/Applications/"*.app; do
          [[ -e "$app" ]] || continue
          local appName
          appName=$(basename "$app")
          local dest="$targetDir/$appName"
          local marker="$markerDir/$appName"

          wantedApps["$appName"]=1

          if [[ ! -f "$marker" ]] || [[ "$(cat "$marker")" != "$src" ]]; then
            echo "Syncing $appName..." >&2
            chmod -R u+w "$dest" 2>/dev/null || true
            rm -rf "$dest"
            /usr/bin/ditto "$app" "$dest"
            echo "$src" > "$marker"
          fi
        done
      }

      ${syncScript}

      # Remove apps no longer in the list
      for app in "$targetDir/"*.app; do
        [[ -e "$app" ]] || continue
        appName=$(basename "$app")
        if [[ -z "''${wantedApps[$appName]:-}" ]]; then
          echo "Removing stale $appName..." >&2
          chmod -R u+w "$app" 2>/dev/null || true
          rm -rf "$app"
          rm -f "$markerDir/$appName"
        fi
      done
    '';
  };
}
