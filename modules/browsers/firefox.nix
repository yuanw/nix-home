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
            "browser.search.region" = "CA";
            "browser.search.countryCode" = "CA";
            "browser.search.isUS" = false;
            "browser.ctrlTab.recentlyUsedOrder" = false;
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
            "devtools.theme" = "dark";
            # Your customized toolbar settings are stored in
            # 'browser.uiCustomization.state'. This tells firefox to sync it between
            # machines. WARNING: This may not work across OSes. Since I use NixOS on
            # all the machines I use Firefox on, this is no concern to me.
            "services.sync.prefs.sync.browser.uiCustomization.state" = true;
            # Enable userContent.css and userChrome.css for our theme modules
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            # Stop creating ~/Downloads!
            # "browser.download.dir" = "${config.user.home}/downloads";
            # Don't use the built-in password manager. A nixos user is more likely
            # using an external one (you are using one, right?).
            "signon.rememberSignons" = false;
            # Do not check if Firefox is the default browser
            "browser.shell.checkDefaultBrowser" = false;
            # Disable the "new tab page" feature and show a blank tab instead
            # https://wiki.mozilla.org/Privacy/Reviews/New_Tab
            # https://support.mozilla.org/en-US/kb/new-tab-page-show-hide-and-customize-top-sites#w_how-do-i-turn-the-new-tab-page-off
            "browser.newtabpage.enabled" = false;
            "browser.newtab.url" = "about:blank";
            # Disable Activity Stream
            # https://wiki.mozilla.org/Firefox/Activity_Stream
            "browser.newtabpage.activity-stream.enabled" = false;
            # Disable new tab tile ads & preload
            # http://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
            # http://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
            # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
            # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
            # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
            "browser.newtabpage.enhanced" = false;
            "browser.newtab.preload" = false;
            "browser.newtabpage.directory.ping" = "";
            "browser.newtabpage.directory.source" = "data:text/plain,{}";
            # Reduce search engine noise in the urlbar's completion window. The
            # shortcuts and suggestions will still work, but Firefox won't clutter
            # its UI with reminders that they exist.
            "browser.urlbar.suggest.searches" = false;
            "browser.urlbar.shortcuts.bookmarks" = false;
            "browser.urlbar.shortcuts.history" = false;
            "browser.urlbar.shortcuts.tabs" = false;
            "browser.urlbar.showSearchSuggestionsFirst" = false;
            # Disable some not so useful functionality.
            "media.videocontrols.picture-in-picture.video-toggle.enabled" =
              false;
            "extensions.htmlaboutaddons.recommendations.enabled" = false;
            "extensions.htmlaboutaddons.discover.enabled" = false;
            "extensions.pocket.enabled" = false;
            "app.normandy.enabled" = false;
            "app.normandy.api_url" = "";
            "extensions.shield-recipe-client.enabled" = false;
            "app.shield.optoutstudies.enabled" = false;
            "reader.parse-on-load.enabled" = false; # "reader view"
            # Disable battery API
            # https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager
            # https://bugzilla.mozilla.org/show_bug.cgi?id=1313580
            "dom.battery.enabled" = false;
            # Disable "beacon" asynchronous HTTP transfers (used for analytics)
            # https://developer.mozilla.org/en-US/docs/Web/API/navigator.sendBeacon
            "beacon.enabled" = false;
            # Disable pinging URIs specified in HTML <a> ping= attributes
            # http://kb.mozillazine.org/Browser.send_pings
            "browser.send_pings" = false;
            # Disable gamepad API to prevent USB device enumeration
            # https://www.w3.org/TR/gamepad/
            # https://trac.torproject.org/projects/tor/ticket/13023
            "dom.gamepad.enabled" = false;
            # Don't try to guess domain names when entering an invalid domain name in URL bar
            # http://www-archive.mozilla.org/docs/end-user/domain-guessing.html
            "browser.fixup.alternate.enabled" = false;
            # Disable telemetry
            # https://wiki.mozilla.org/Platform/Features/Telemetry
            # https://wiki.mozilla.org/Privacy/Reviews/Telemetry
            # https://wiki.mozilla.org/Telemetry
            # https://www.mozilla.org/en-US/legal/privacy/firefox.html#telemetry
            # https://support.mozilla.org/t5/Firefox-crashes/Mozilla-Crash-Reporter/ta-p/1715
            # https://wiki.mozilla.org/Security/Reviews/Firefox6/ReviewNotes/telemetry
            # https://gecko.readthedocs.io/en/latest/browser/experiments/experiments/manifest.html
            # https://wiki.mozilla.org/Telemetry/Experiments
            # https://support.mozilla.org/en-US/questions/1197144
            # https://firefox-source-docs.mozilla.org/toolkit/components/telemetry/telemetry/internals/preferences.html#id1
            "toolkit.telemetry.enabled" = false;
            "toolkit.telemetry.unified" = false;
            "toolkit.telemetry.archive.enabled" = false;
            "experiments.supported" = false;
            "experiments.enabled" = false;
            "experiments.manifest.uri" = "";
            # Disable health reports (basically more telemetry)
            # https://support.mozilla.org/en-US/kb/firefox-health-report-understand-your-browser-perf
            # https://gecko.readthedocs.org/en/latest/toolkit/components/telemetry/telemetry/preferences.html
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.healthreport.service.enabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
          };

        };
      };
    };
  };
}
