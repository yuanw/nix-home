# Shell launcher for Chromium installed via environment.casks (nix-casks).
# Default adds --disable-features=Glic to avoid startup crashes with some builds;
# see https://github.com/ungoogled-software/ungoogled-chromium/issues/3746
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.modules.browsers.nixCasksChromiumWrap;
in
{
  options.modules.browsers.nixCasksChromiumWrap = {
    enable = mkEnableOption "PATH wrapper that launches Nix Casks Chromium with extra flags";
    executable = mkOption {
      type = types.str;
      default = "/Applications/Nix Casks/Chromium.app/Contents/MacOS/Chromium";
      description = ''
        The real Chromium binary inside the .app bundle (after `darwin-rebuild` syncs Nix Casks).
      '';
    };
    extraArgs = mkOption {
      type = types.listOf types.str;
      default = [ "--disable-features=Glic" ];
      description = ''
        Flags inserted before any arguments you pass. The default mitigates a known
        startup failure; see
        https://github.com/ungoogled-software/ungoogled-chromium/issues/3746
      '';
    };
    commandName = mkOption {
      type = types.str;
      default = "chromium-nix-casks";
      description = "Name of the script on PATH (under the default profile).";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username}.home.packages = [
      (pkgs.writeShellScriptBin cfg.commandName ''
        exec ${escapeShellArg cfg.executable} ${escapeShellArgs cfg.extraArgs} "$@"
      '')
    ];
  };
}
