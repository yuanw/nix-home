# LibreWolf — shares Home Manager config with Firefox (gecko-home.nix).
{
  config,
  lib,
  pkgs,
  inputs,
  hostname,
  ...
}:

with lib;
let
  cfg = config.modules.browsers.librewolf;
  firefoxCfg = config.modules.browsers.firefox;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  profilesPath = if isDarwin then "Library/Application Support/LibreWolf/Profiles" else ".librewolf";
  keybindingsCfg =
    if firefoxCfg.enable or false then
      firefoxCfg.keybindings
    else
      {
        enable = false;
        settings = { };
      };
in
{
  options.modules.browsers.librewolf = {
    enable = mkEnableOption "librewolf";
    pkg = mkOption {
      type = with types; nullOr package;
      default = pkgs.librewolf;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} =
      hm@{ pkgs, osConfig, ... }:
      import ./gecko-home.nix {
        inherit
          lib
          pkgs
          inputs
          hostname
          config
          osConfig
          hm
          isDarwin
          profilesPath
          keybindingsCfg
          ;
        program = "librewolf";
        cfg = cfg;
      };
  };
}
