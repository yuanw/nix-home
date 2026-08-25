# LibreWolf browser configuration.
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
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  profilesPath = if isDarwin then "Library/Application Support/LibreWolf/Profiles" else ".librewolf";
in
{
  options.modules.browsers.librewolf = {
    enable = mkEnableOption "librewolf";
    pkg = mkOption {
      type = with types; nullOr package;
      default = pkgs.librewolf;
    };
    darwinExtraPolicies = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Extra enterprise policies merged into the macOS Nix cask build
        (see modules/browsers/librewolf-darwin.nix).
      '';
    };
    keybindings = {
      enable = mkEnableOption "LibreWolf extension keybindings management" // {
        default = true;
      };
      settings = mkOption {
        type = types.attrs;
        default = { };
        example = literalExpression ''
          {
            # Extension ID for Sidebery
            "{3c078156-979c-498b-8990-85f7987dd929}" = {
              "switch_to_panel_0" = { shortcut = "Ctrl+1"; };
              "switch_to_panel_1" = { shortcut = "Ctrl+2"; };
            };
            # Extension ID for Dark Reader
            "addon@darkreader.org" = {
              "toggle" = { shortcut = "Alt+Shift+D"; };
            };
          }
        '';
        description = ''
          Extension keybindings in format: { extensionId.commands.commandName = { shortcut = "..."; } }
          (merged into extension-settings.json as each command's precedenceList[].value).
          Use `about:support` and click "Copy Raw Data to Clipboard" to find extension IDs.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.keybindings.enable) {
      # Sidebery defaults (same shape as upstream keymaps.nix):
      # https://github.com/js0ny/nixcfgs/blob/cac17a65f58a2a90a9c0462579042bf79dfa8c46/home/programs/browsers/firefox/addons/sidebery/keymaps.nix
      modules.browsers.librewolf.keybindings.settings = mkDefault {
        "{3c078156-979c-498b-8990-85f7987dd929}" = {
          switch_to_panel_0 = {
            shortcut = "Ctrl+1";
          };
          switch_to_panel_1 = {
            shortcut = "Ctrl+2";
          };
          switch_to_panel_2 = {
            shortcut = "Ctrl+3";
          };
          switch_to_panel_3 = {
            shortcut = "Ctrl+4";
          };
          next_panel = {
            shortcut = "";
          };
          prev_panel = {
            shortcut = "";
          };
          switch_to_prev_tab = {
            shortcut = "Alt+H";
          };
          switch_to_next_tab = {
            shortcut = "Alt+L";
          };
        };
        # userchrome-toggle-extended: shortcuts as in ~/Library/Application Support/LibreWolf/Profiles/home/extension-settings.json (macOS).
        "userchrome-toggle-extended@n2ezr.ru" = {
          "1" = {
            shortcut = "MacCtrl+Shift+H";
          };
          "2" = {
            shortcut = "MacCtrl+Shift+T";
          };
          "3" = {
            shortcut = "MacCtrl+Shift+N";
          };
        };
      };
    })
    (mkIf cfg.enable {
      home-manager.users.${config.my.username} =
        hm@{ pkgs, osConfig, ... }:
        import ./librewolf-home.nix {
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
          keybindingsCfg = cfg.keybindings;
          program = "librewolf";
          cfg = cfg;
        };
    })
  ];
}
