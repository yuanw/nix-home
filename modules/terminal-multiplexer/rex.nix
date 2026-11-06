{
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}:
let
  cfg = config.modules.rex;
in
with lib;
{
  options.modules.rex = {
    enable = mkEnableOption "Rex terminal multiplexer";
  };

  config = mkIf cfg.enable (
    {
      home-manager.users.${config.my.username}.home.packages = [
        pkgs.rex
      ];
    }
    # optionalAttrs, not mkIf: this module is in myModules.common, and the
    # `system.defaults` option name alone fails evaluation on NixOS hosts.
    // optionalAttrs isDarwin {
      # Rex reads these at launch and rewrites the plist when it quits, so quit
      # Rex before darwin-rebuild or the write is lost.
      system.defaults.CustomUserPreferences."com.superlogical.rex" = {
        # Must name an installed face exactly; the same value Alacritty uses.
        "Workspace.terminalFontName" = config.my.monoFont;
        # A Nix float -- nix-darwin throws on the string "16.0".
        "Workspace.terminalFontPointSize" = 18.0;
        "Workspace.terminalAllowsLigatures" = true;
      };
    }
  );
}
