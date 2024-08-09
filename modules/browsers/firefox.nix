# https://github.com/hlissner/dotfiles/blob/master/modules/desktop/browsers/firefox.nix
# should try out this https://github.com/mlyxshi/flake/blob/main/config/firefox/policy.nix
# https://github.com/mozilla/policy-templates
{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.browsers.firefox;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  profilesPath =
    if isDarwin then "Library/Application Support/Firefox/Profiles" else ".mozilla/firefox";
in
{
  options.modules.browsers.firefox = {
    enable = mkEnableOption "firefox";
    pkg = mkOption {
      type = types.package;
      default = pkgs.firefox;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home = {
        file."startpage".source = ./startpage;
        packages = [
          pkgs.tridactyl-native
        ];
        file."${profilesPath}/home/chrome".source = "${inputs.shy-fox}/chrome";
      };
      programs.firefox.enable = true;
      programs.firefox.package = cfg.pkg;
      # https://mozilla.github.io/policy-templates/
      programs.firefox.policies = {
        DontCheckDefaultBrowser = true;
        DisablePocket = true;
        DisableAppUpdate = true;
      };
      # programs.firefox.nativeMessagingHosts = [
      #   pkgs.tridactyl-native
      # ];
      programs.firefox.profiles = {
        home = {
          name = "home";
          #https://gitlab.com/rycee/nur-expressions/-/blob/master/pkgs/firefox-addons/generated-firefox-addons.nix
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            tridactyl
            ublock-origin
            ghosttext
            privacy-badger
            leechblock-ng
            kagi-search
            userchrome-toggle-extended
            sidebery
            # https://addons.mozilla.org/api/v5/addons/search/?q=mtab
            (buildFirefoxXpiAddon {
              pname = "mtab";
              version = "1.3.7";
              addonId = "contact@maxhu.dev";
              url = "https://addons.mozilla.org/firefox/downloads/file/4330262/mtab-1.3.7.xpi";
              sha256 = "90a87a541a2a07838398aa30c916968ec6b39b8190a64e60cb365dacb4e2fa67";
              meta = with lib;
                {
                  homepage = "https://github.com/maxhu08/mtab";
                  description = "a simple configurable new tab extension";
                  license = licenses.mit;
                  mozPermissions = [
                    "bookmarks"
                  ];
                  platforms = platforms.all;
                };

            })
          ];

          search = {
            default = "Kagi";
            engines = {
              "Google".metaData.hidden = true;
              "Bing".metaData.hidden = true;
              "eBay".metaData.hidden = true;
              "Amazon".metaData.hidden = true;
              "Nix Packages" = {
                urls = [{
                  template = "https://search.nixos.org/packages";
                  params = [
                    {
                      name = "type";
                      value = "packages";
                    }
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }];
                icon =
                  "''${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@np" ];
              };
              # "NixOS Wiki" = {
              #   urls = [{
              #     template =
              #       "https://nixos.wiki/index.php?search={searchTerms}";
              #   }];
              #   iconUpdateURL = "https://nixos.wiki/favicon.png";
              #   updateInterval = 24 * 60 * 60 * 1000; # every day
              #   definedAliases = [ "@nw" ];
              # };
            };
          };
          # userChrome = ''
          #   *{font-size: 18px !important;
          #                     }
          # '';
          # https://github.com/arkenfox/user.js/blob/master/user.js
          settings = {
            # ratio to enlarge default 96 pixes per inch 1.5 gives 50% enlargement
            # "layout.css.devPixelsPerPx" = "2.0";
            # Default to dark theme in DevTools panel
            "devtools.theme" = "dark";
            "app.update.auto" = false;
            "app.update.service.enabled" = false;
            # Enable ETP for decent security (makes firefox containers and many
            # common security/privacy add-ons redundant).
            "browser.contentblocking.category" = "strict";
            "privacy.donottrackheader.enabled" = true;
            "privacy.donottrackheader.value" = 1;
            "privacy.purge_trackers.enabled" = true;
            "browser.startup.homepage" = "https://yuanwang.dev/";
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
            # "browser.newtab.url" = "about:blank";
            "browser.newtab.url" = "yuanwang.dev";
            # Disable Activity Stream
            # https://wiki.mozilla.org/Firefox/Activity_Stream
            # Disable new tab tile ads & preload
            # http://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
            # http://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
            # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
            # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
            # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
            # "browser.newtabpage.directory.source" = "data:text/plain,{}";
            # Reduce search engine noise in the urlbar's completion window. The
            # shortcuts and suggestions will still work, but Firefox won't clutter
            # its UI with reminders that they exist.
            "browser.urlbar.suggest.searches" = false;
            "browser.urlbar.shortcuts.bookmarks" = false;
            "browser.urlbar.shortcuts.history" = false;
            "browser.urlbar.shortcuts.tabs" = false;
            "browser.urlbar.showSearchSuggestionsFirst" = false;
            # Disable Activity Stream
            # https://wiki.mozilla.org/Firefox/Activity_Stream
            "browser.newtabpage.activity-stream.enabled" = false;
            "browser.newtabpage.activity-stream.telemetry" = false;
            # Disable new tab tile ads & preload
            # http://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
            # http://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
            # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
            # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
            # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
            "browser.newtabpage.enhanced" = false;
            "browser.newtabpage.introShown" = true;
            "browser.newtab.preload" = false;
            "browser.newtabpage.directory.ping" = "";
            # Reduce search engine noise in the urlbar's completion window. The
            # shortcuts and suggestions will still work, but Firefox won't clutter
            # its UI with reminders that they exist.
            "browser.urlbar.speculativeConnect.enabled" = false;
            # https://bugzilla.mozilla.org/1642623
            "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
            # https://blog.mozilla.org/data/2021/09/15/data-and-firefox-suggest/
            "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
            "browser.urlbar.suggest.quicksuggest.sponsored" = false;
            # Show whole URL in address bar
            "browser.urlbar.trimURLs" = false;
            # Disable some not so useful functionality.
            "browser.disableResetPrompt" =
              true; # "Looks like you haven't started Firefox in a while."
            "browser.onboarding.enabled" =
              false; # "New to Firefox? Let's get started!" tour
            "browser.aboutConfig.showWarning" =
              false; # Warning when opening about:config
            "media.videocontrols.picture-in-picture.video-toggle.enabled" =
              false;
            "extensions.pocket.enabled" = false;
            "extensions.shield-recipe-client.enabled" = false;
            "reader.parse-on-load.enabled" = false; # "reader view"

            # Security-oriented defaults
            "security.family_safety.mode" = 0;
            # https://blog.mozilla.org/security/2016/10/18/phasing-out-sha-1-on-the-public-web/
            "security.pki.sha1_enforcement_level" = 1;
            # https://github.com/tlswg/tls13-spec/issues/1001
            "security.tls.enable_0rtt_data" = false;
            # Use Mozilla geolocation service instead of Google if given permission
            "geo.provider.network.url" =
              "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";
            "geo.provider.use_gpsd" = false;
            # https://support.mozilla.org/en-US/kb/extension-recommendations
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr" = false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" =
              false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" =
              false;
            "extensions.htmlaboutaddons.recommendations.enabled" = false;
            "extensions.htmlaboutaddons.discover.enabled" = false;
            "extensions.getAddons.showPane" = false; # uses Google Analytics
            "browser.discovery.enabled" = false;
            # Disable some not so useful functionality.
            "app.normandy.enabled" = false;
            "app.normandy.api_url" = "";
            "app.shield.optoutstudies.enabled" = false;
            # Reduce File IO / SSD abuse
            # Otherwise, Firefox bombards the HD with writes. Not so nice for SSDs.
            # This forces it to write every 30 minutes, rather than 15 seconds.
            "browser.sessionstore.interval" = "1800000";
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
            "browser.ping-centre.telemetry" = false;
            # https://mozilla.github.io/normandy/

            # Disable health reports (basically more telemetry)
            # https://support.mozilla.org/en-US/kb/firefox-health-report-understand-your-browser-perf
            # https://gecko.readthedocs.org/en/latest/toolkit/components/telemetry/telemetry/preferences.html
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.healthreport.service.enabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;

            # Disable crash reports
            "breakpad.reportURL" = "";
            "browser.tabs.crashReporting.sendReport" = false;
            "browser.crashReports.unsubmittedCheck.autoSubmit2" =
              false; # don't submit backlogged reports

            # Disable Form autofill
            # https://wiki.mozilla.org/Firefox/Features/Form_Autofill
            "browser.formfill.enable" = false;
            "extensions.formautofill.addresses.enabled" = false;
            "extensions.formautofill.available" = "off";
            "extensions.formautofill.creditCards.available" = false;
            "extensions.formautofill.creditCards.enabled" = false;
            "extensions.formautofill.heuristics.enabled" = false;
            # shyfox
            ## Fill SVG Color
            "svg.context-properties.content.enabled" = true;
            ## CSS's `:has()` selector
            "layout.css.has-selector.enabled" = true;
          };

        };
      };
    };
  };
}
