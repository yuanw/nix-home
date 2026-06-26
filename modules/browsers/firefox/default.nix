# https://github.com/hlissner/dotfiles/blob/master/modules/desktop/browsers/firefox.nix
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
  cfg = config.modules.browsers.firefox;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  profilesPath =
    if isDarwin then "Library/Application Support/Firefox/Profiles" else ".config/mozilla/firefox";
in
{
  options.modules.browsers.firefox = {
    enable = mkEnableOption "firefox";
    pkg = mkOption {
      type = with types; nullOr package;
      default = pkgs.firefox;
    };
    keybindings = {
      enable = mkEnableOption "Firefox extension keybindings management" // {
        default = true;
      };
      settings = mkOption {
        type = types.attrs;
        default = { };
        description = ''
          Extension keybindings: { extensionId.commands.commandName = { shortcut = "..."; } }
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.keybindings.enable) {
      modules.browsers.firefox.keybindings.settings = mkDefault {
        "{3c078156-979c-498b-8990-85f7987dd929}" = {
          switch_to_panel_0.shortcut = "Ctrl+1";
          switch_to_panel_1.shortcut = "Ctrl+2";
          switch_to_panel_2.shortcut = "Ctrl+3";
          switch_to_panel_3.shortcut = "Ctrl+4";
          next_panel.shortcut = "";
          prev_panel.shortcut = "";
          switch_to_prev_tab.shortcut = "Alt+H";
          switch_to_next_tab.shortcut = "Alt+L";
        };
        "userchrome-toggle-extended@n2ezr.ru" = {
          "1".shortcut = "MacCtrl+Alt+H";
          "2".shortcut = "MacCtrl+Alt+T";
          "3".shortcut = "MacCtrl+Alt+N";
        };
      };
    })
    (mkIf cfg.enable {
      home-manager.users.${config.my.username} =
        hm@{ pkgs, osConfig, ... }:
        import ../gecko-home.nix {
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
            ;
          program = "firefox";
          cfg = cfg;
          keybindingsCfg = cfg.keybindings;
        };
    })
  ];
}
