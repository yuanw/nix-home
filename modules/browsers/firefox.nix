# https://github.com/hlissner/dotfiles/blob/master/modules/desktop/browsers/firefox.nix
{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.browsers.firefox;
in {
  options.modules.browsers.firefox = {
    enable = mkEnableOption "firefox";
    pkg = mkOption {
      type = types.package;
      default = pkgs.firefox;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      programs.firefox.enable = true;
      programs.firefox.package = cfg.pkg;
      programs.firefox.extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        tridactyl
        ublock-origin
        https-everywhere
        privacy-badger
        leechblock-ng
      ];
      programs.firefox.profiles = {
        home = {
          id = 0;
          # userChrome = (builtins.readFile (pkgs.substituteAll {
          #   name = "homeUserChrome";
          #   src = ./userChrome.css;
          #   tabLineColour = "#5e81ac";
          # }));
          settings = {
            "app.update.auto" = false;
            "browser.startup.homepage" = "https://lobste.rs";
            "browser.search.region" = "GB";
            "browser.search.countryCode" = "GB";
            "browser.search.isUS" = false;
            "browser.shell.checkDefaultBrowser" = false;
            "browser.ctrlTab.recentlyUsedOrder" = false;
            "browser.newtabpage.enabled" = false;
            "browser.newtabpage.enhanced" = false;
            "browser.newtab.preload" = false;
            "browser.newtabpage.directory.ping" = "";
            "browser.newtabpage.directory.source" = "data:text/plain,{}";
            "browser.bookmarks.showMobileBookmarks" = true;
            "browser.uidensity" = 1;
            "browser.urlbar.placeholderName" = "DuckDuckGo";
            "browser.urlbar.update1" = true;
            "distribution.searchplugins.defaultLocale" = "en-CA";
            "general.useragent.locale" = "en-CA";
            "identity.fxaccounts.account.device.name" =
              config.networking.hostName;
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
            "reader.color_scheme" = "sepia";
            "services.sync.declinedEngines" = "addons,passwords,prefs";
            "services.sync.engine.addons" = false;
            "services.sync.engineStatusChanged.addons" = true;
            "services.sync.engine.passwords" = false;
            "services.sync.engine.prefs" = false;
            "services.sync.engineStatusChanged.prefs" = true;
            "signon.rememberSignons" = false;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          };

        };
      };
    };
  };
}
