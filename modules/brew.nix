{ config, lib, pkgs, localConfig, ... }:
############
# Homebrew #
############
with lib;
let cfg = config.modules.brew;
in {
  options.modules.brew = {
    enable = mkEnableOption "brew";
    taps = mkOption {
      type = types.listOf types.str;
      default = [ "homebrew/core" "homebrew/cask" ];
    };
    brews = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
    casks = mkOption {
      type = types.listOf types.str;
      default = [ "firefox" ];
    };
    masApps = mkOption {
      type = with types; attrsOf ints.positive;
      default = {};
    };
    extraConfig = mkOption {
      type = types.lines;
      default = ''
        cask "firefox", args: { language: "en-CA" }
      '';
    };
  };

  config = mkIf cfg.enable {
    homebrew.enable = true;
    homebrew.autoUpdate = true;
    homebrew.cleanup = "zap";
    homebrew.global.brewfile = true;
    homebrew.global.noLock = true;
    homebrew.taps = cfg.taps;
    homebrew.brews = cfg.brews;
    homebrew.casks = cfg.casks;
    homebrew.masApps = cfg.masApps;
  };
}
