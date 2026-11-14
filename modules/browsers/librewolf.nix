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

  # Adapted from NaviOS modules/features/firefox/firefox-ublock.nix. Settings must nest
  # under adminSettings (uBO's backup format) -- see assets/managed_storage.json in the xpi.
  # Dropped from the original: the ExtensionSettings AMO install block (would fight the
  # NUR/home-manager install), the "adguard-spyware" token (retired upstream, resolves to
  # nothing), userFiltersTrusted, and the DEU/FRA/NLD/RUS lists (host locale is en_US).
  # ublock-quick-fixes is excluded: its YouTube anti-abuse scriptlets (serverContract /
  # trusted-edit-inbound-object on attestationRequest) make YouTube reject the stream with
  # ec.auth "This content isn't available".
  ublockImportedLists = [
    "https://filters.adtidy.org/extension/ublock/filters/3.txt"
    "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/ClearURLs%20for%20uBo/clear_urls_uboified.txt"
    "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/Dandelion%20Sprout's%20Anti-Malware%20List.txt"
    "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/LegitimateURLShortener.txt"
    "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/BrowseWebsitesWithoutLoggingIn.txt"
    "https://raw.githubusercontent.com/yokoffing/filterlists/main/privacy_essentials.txt"
    "https://raw.githubusercontent.com/yokoffing/filterlists/main/annoyance_list.txt"
    "https://raw.githubusercontent.com/liamengland1/miscfilters/master/antipaywall.txt"
    "https://raw.githubusercontent.com/iam-py-test/my_filters_001/main/antimalware.txt"
  ];

  ublockAdminPolicies = {
    "3rdparty".Extensions."uBlock0@raymondhill.net" = {
      # [name value] pairs, locked as admin-set in the Advanced settings pane.
      advancedSettings = [
        # Twitch ad player patch, LeechBlock already gates twitch.tv.
        [
          "userResourcesLocation"
          "https://raw.githubusercontent.com/pixeltris/TwitchAdSolutions/master/video-swap-new/video-swap-new-ublock-origin.js"
        ]
      ];
      # Pass an object, not a JSON string: Firefox expands a policy string into a
      # character-indexed object on the way to browser.storage.managed, which makes
      # uBO's restoreAdminSettings silently apply nothing.
      adminSettings = {
        userSettings = {
          advancedUserEnabled = true;
          cloudStorageEnabled = false;
          popupPanelSections = 31;
          uiTheme = "dark";
          importedLists = ublockImportedLists;
          externalLists = lib.concatStringsSep "\n" ublockImportedLists;
        };
        selectedFilterLists = [
          "user-filters"
          "ublock-filters"
          "ublock-badware"
          "ublock-privacy"
          "ublock-unbreak"
          "easylist"
          "easyprivacy"
          "adguard-generic"
          "adguard-mobile"
          "adguard-spyware-url"
          "block-lan"
          "urlhaus-1"
          "curben-phishing"
          "plowe-0"
          "dpollock-0"
          "LegitimateURLShortener"
          "fanboy-cookiemonster"
          "ublock-cookies-easylist"
          "adguard-cookies"
          "ublock-cookies-adguard"
          "fanboy-social"
          "adguard-social"
          "fanboy-thirdparty_social"
          "easylist-chat"
          "easylist-newsletters"
          "easylist-notifications"
          "easylist-annoyances"
          "adguard-mobile-app-banners"
          "adguard-other-annoyances"
          "adguard-popup-overlays"
          "adguard-widgets"
          "ublock-annoyances"
        ]
        ++ ublockImportedLists;
      };
    };
  };
in
{
  options.modules.browsers.librewolf = {
    enable = mkEnableOption "librewolf";
    pkg = mkOption {
      type = with types; nullOr package;
      default = pkgs.librewolf;
    };
    spookfox = {
      enable = mkEnableOption ''
        Spookfox add-on, which bridges LibreWolf and Emacs over websockets
        (see modules/editor/emacs/features/spookfox.nix)
      '';
    };
    darwinExtraPolicies = mkOption {
      type = types.attrs;
      default = { };
      description = ''
        Extra enterprise policies merged into the macOS Home Manager app
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
      modules.browsers.librewolf.darwinExtraPolicies = ublockAdminPolicies;
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
