# https://github.com/hlissner/dotfiles/blob/master/modules/desktop/browsers/firefox.nix
# should try out this https://github.com/mlyxshi/flake/blob/main/config/firefox/policy.nix
# https://github.com/mozilla/policy-templates
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

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
        file."${profilesPath}/home/chrome".source = "${inputs.shy-fox}/chrome";
      };
      programs.firefox = {
        enable = true;
        betterfox = {
          enable = true;
          #version = "128.0"; # Set version here, defaults to main branch
        };
      };
      programs.firefox.package = cfg.pkg;
      # https://mozilla.github.io/policy-templates/
      # ~/Applications/Home Manager Apps/Firefox.app/Contents/Resources/distribution/policies.json
      programs.firefox.policies = {
        DontCheckDefaultBrowser = true;
        DisablePocket = true;
        DisableAppUpdate = true;
        DisableTelemetry = true;
        SearchEngines = {
          PreventInstalls = true;
          Add = [
            {
              Name = "Kagi";
              URLTemplate = "https://kagi.com/search?q={searchTerms}";
              Method = "GET";
              IconURL = "https://kagi.com/asset/405c65f/favicon-32x32.png?v=49886a9a8f55fd41f83a89558e334f673f9e25cf";
              Description = "Kagi Search";
            }

            {
              Name = "Nix Packages";
              Description = "Nix package Search";
              URLTemplate = "https://search.nixos.org/packages?type=packages&query={searchTerms}";
              Method = "GET";
              IconURL = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              Alias = "np";
            }

          ];
          Remove = [
            "Bing"
          ];
          Default = "Kagi";
        };
        SearchSuggestEnabled = false;
      };
      # TODO: does this work
      # https://github.com/llakala/nixos/blob/ffc71bb84cb95dd813795d4cb0beb99cebf8a4e0/base/software/home/firefox.nix
      programs.firefox.policies.Preferences = {
        "browser.in-content.dark-mode" = true; # Use dark mode
        "ui.systemUsesDarkTheme" = true;
        "extensions.autoDisableScopes" = 0; # Automatically enable extensions
        "extensions.update.enabled" = false;
        "widget.use-xdg-desktop-portal.file-picker" = 1; # Use new gtk file picker instead of legacy one
      };
      # programs.firefox.nativeMessagingHosts = [
      #   pkgs.tridactyl-native
      # ];
      programs.firefox.profiles = {
        #~/Library/Application Support/Firefox
        home = {
          betterfox.enable = true;
          name = "home";
          extensions = {
            force = true;
            #https://gitlab.com/rycee/nur-expressions/-/blob/master/pkgs/firefox-addons/generated-firefox-addons.nix
            packages = with pkgs.nur.repos.rycee.firefox-addons; [
              # kagi-search
              darkreader
              ghosttext
              history-cleaner
              leechblock-ng
              mtab
              privacy-badger
              sidebery
              ublock-origin
              userchrome-toggle-extended
              #vimium-c
              tridactyl
            ];

            # Addon IDs can be found in about:support#addons
            # https://github.com/kittywitch/nixfiles/blob/026208109467e27b52953382074d24ce8acadd0a/home/profiles/graphical/floorp/mtab.nix
            # https://github.com/spl3g/nixfiles/blob/7824eb7aeff2c659f3cdf5306ed30c3dd770d75e/home-manager/homeModules/firefox.nix#L29
            # https://github.com/nuclearcodecat/shimmer/blob/main/sidebery.json
            # To change: Customize ui, copy it from about:config and paste here.
            # browser.uiCustomization.state = ''
            #   {"placements":{"widget-overflow-fixed-list":[],"unified-extensions-area":["_7be2ba16-0f1e-4d93-9ebc-5164397477a9_-browser-action","_ea4204c0-3209-4116-afd2-2a208e21a779_-browser-action","_531906d3-e22f-4a6c-a102-8057b88a1a63_-browser-action"],"nav-bar":["back-button","forward-button","customizableui-special-spring1","urlbar-container","stop-reload-button","customizableui-special-spring2","save-to-pocket-button","downloads-button","unified-extensions-button","_3c078156-979c-498b-8990-85f7987dd929_-browser-action","ublock0_raymondhill_net-browser-action","addon_darkreader_org-browser-action"],"toolbar-menubar":["menubar-items"],"TabsToolbar":["firefox-view-button","tabbrowser-tabs","new-tab-button","alltabs-button"],"PersonalToolbar":["import-button","personal-bookmarks"]},"seen":["save-to-pocket-button","developer-button","_3c078156-979c-498b-8990-85f7987dd929_-browser-action","ublock0_raymondhill_net-browser-action","_7be2ba16-0f1e-4d93-9ebc-5164397477a9_-browser-action","_ea4204c0-3209-4116-afd2-2a208e21a779_-browser-action","addon_darkreader_org-browser-action","_531906d3-e22f-4a6c-a102-8057b88a1a63_-browser-action"],"dirtyAreaCache":["nav-bar","PersonalToolbar","toolbar-menubar","TabsToolbar","unified-extensions-area"],"currentVersion":20,"newElementCount":3}
            #        '';
            settings = with pkgs.nur.repos.rycee.firefox-addons; {
              "${ublock-origin.addonId}".settings = {
                userSettings = rec {
                  advancedUserEnabled = true;
                  cloudStorageEnabled = false;
                  # collapseBlocked = false;
                  uiAccentCustom = true;
                  uiAccentCustom0 = "#ACA0F7";
                  externalLists = pkgs.lib.concatStringsSep "\n" importedLists;
                  importedLists = [
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/anti.piracy.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/doh-vpn-proxy-bypass.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/dyndns.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/fake.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/gambling.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/hoster.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/nsfw.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/pro.mini.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/spam-tlds-ublock.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/tif.txt"
                    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/ultimate.txt"
                  ];
                  largeMediaSize = 250;
                  # popupPanelSections = 31;
                  tooltipsDisabled = true;
                };
                dynamicFilteringString = ''
                  no-cosmetic-filtering: * true
                  no-cosmetic-filtering: appleid.apple.com false
                  no-cosmetic-filtering: bing.com false
                  no-cosmetic-filtering: cnn.com false
                  no-cosmetic-filtering: google.com false
                  no-cosmetic-filtering: www.notion.com false
                  no-cosmetic-filtering: www.notion.so false
                  no-cosmetic-filtering: old.reddit.com false
                  no-cosmetic-filtering: slack.com false
                  no-cosmetic-filtering: kadena-io.slack.com false
                  no-cosmetic-filtering: twitch.tv false
                  no-cosmetic-filtering: youtube.com false
                  no-csp-reports: * true
                  no-large-media: * true
                  no-large-media: www.amazon.com false
                  no-large-media: appleid.apple.com false
                  no-large-media: login.bmwusa.com false
                  no-large-media: www.ftb.ca.gov false
                  no-large-media: www.notion.com false
                  no-large-media: www.notion.so false
                  no-large-media: old.reddit.com false
                  no-large-media: client.schwab.com false
                  no-large-media: sws-gateway-nr.schwab.com false
                  no-large-media: slack.com false
                  no-large-media: kadena-io.slack.com false
                  no-large-media: www.youtube.com false
                  no-remote-fonts: * true
                  no-remote-fonts: www.amazon.com false
                  no-remote-fonts: appleid.apple.com false
                  no-remote-fonts: login.bmwusa.com false
                  no-remote-fonts: www.ftb.ca.gov false
                  no-remote-fonts: docs.google.com false
                  no-remote-fonts: drive.google.com false
                  no-remote-fonts: gemini.google.com false
                  no-remote-fonts: notebooklm.google.com false
                  no-remote-fonts: www.google.com false
                  no-remote-fonts: kadena.latticehq.com false
                  no-remote-fonts: www.notion.com false
                  no-remote-fonts: www.notion.so false
                  no-remote-fonts: usa.onlinesrp.org false
                  no-remote-fonts: old.reddit.com false
                  no-remote-fonts: client.schwab.com false
                  no-remote-fonts: sws-gateway-nr.schwab.com false
                  no-remote-fonts: slack.com false
                  no-remote-fonts: app.slack.com false
                  no-remote-fonts: kadena-io.slack.com false
                  no-remote-fonts: www.youtube.com false
                  * * 3p-frame block
                  * * 3p-script block
                  * cloudflare.com * noop
                  www.amazon.com * 3p noop
                  www.amazon.com * 3p-frame noop
                  www.amazon.com * 3p-script noop
                  console.anthropic.com * 3p-frame noop
                  console.anthropic.com * 3p-script noop
                  appleid.apple.com * 3p-frame noop
                  appleid.apple.com * 3p-script noop
                  app.asana.com * 3p-frame noop
                  app.asana.com * 3p-script noop
                  behind-the-scene * * noop
                  behind-the-scene * 1p-script noop
                  behind-the-scene * 3p noop
                  behind-the-scene * 3p-frame noop
                  behind-the-scene * 3p-script noop
                  behind-the-scene * image noop
                  behind-the-scene * inline-script noop
                  app01.us.bill.com * 3p-frame noop
                  app01.us.bill.com * 3p-script noop
                  login.bmwusa.com * 3p-frame noop
                  login.bmwusa.com * 3p-script noop
                  www.facebook.com * 3p noop
                  www.facebook.com * 3p-frame noop
                  www.facebook.com * 3p-script noop
                  www.fidium.net * 3p-frame noop
                  www.fidium.net * 3p-script noop
                  file-scheme * 3p-frame noop
                  file-scheme * 3p-script noop
                  github.com * 3p noop
                  github.com * 3p-frame noop
                  github.com * 3p-script noop
                  accounts.google.com * 3p-frame noop
                  accounts.google.com * 3p-script noop
                  docs.google.com * 3p-frame noop
                  docs.google.com * 3p-script noop
                  drive.google.com * 3p noop
                  drive.google.com * 3p-frame noop
                  drive.google.com * 3p-script noop
                  notebooklm.google.com * 3p noop
                  notebooklm.google.com * 3p-frame noop
                  notebooklm.google.com * 3p-script noop
                  huggingface.co * 3p-frame noop
                  huggingface.co * 3p-script noop
                  kadena.latticehq.com * 3p-frame noop
                  kadena.latticehq.com * 3p-script noop
                  www.linkedin.com * 3p noop
                  www.notion.com * 3p-frame noop
                  www.notion.com * 3p-script noop
                  www.notion.so * 3p-frame noop
                  www.notion.so * 3p-script noop
                  old.reddit.com * 3p noop
                  old.reddit.com * 3p-frame noop
                  old.reddit.com * 3p-script noop
                  www.reddit.com * 3p noop
                  www.reddit.com * 3p-frame noop
                  www.reddit.com * 3p-script noop
                  respected-meat-54f.notion.site * 3p noop
                  myprofile.saccounty.gov * 3p-frame noop
                  myprofile.saccounty.gov * 3p-script noop
                  myutilities.saccounty.gov * 3p-frame noop
                  myutilities.saccounty.gov * 3p-script noop
                  client.schwab.com * 3p-frame noop
                  client.schwab.com * 3p-script noop
                  sws-gateway-nr.schwab.com * 3p-frame noop
                  sws-gateway-nr.schwab.com * 3p-script noop
                  slack.com * 3p-frame noop
                  slack.com * 3p-script noop
                  app.slack.com * 3p noop
                  app.slack.com * 3p-frame noop
                  app.slack.com * 3p-script noop

                  www.youtube.com * 3p-frame noop
                  www.youtube.com * 3p-script noop
                '';
                urlFilteringString = "";
                hostnameSwitchesString = ''
                  no-remote-fonts: * true
                  no-large-media: * true
                  no-csp-reports: * true
                  no-remote-fonts: www.ftb.ca.gov false
                  no-large-media: www.ftb.ca.gov false
                  no-remote-fonts: app.slack.com false
                  no-remote-fonts: notebooklm.google.com false
                '';
                userFilters = "";
                selectedFilterLists = [
                  "user-filters"
                  "ublock-filters"
                  "ublock-badware"
                  "ublock-privacy"
                  "ublock-quick-fixes"
                  "ublock-unbreak"
                  "easylist"
                  "easyprivacy"
                  "adguard-spyware"
                  "adguard-spyware-url"
                  "urlhaus-1"
                  "plowe-0"
                  "fanboy-cookiemonster"
                  "ublock-cookies-easylist"
                  "fanboy-social"
                  "easylist-chat"
                  "easylist-newsletters"
                  "easylist-notifications"
                  "easylist-annoyances"
                  "ublock-annoyances"
                  "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/hoster.txt"
                  "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/fake.txt"
                  "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/pro.mini.txt"
                  "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/spam-tlds-ublock.txt"
                ];
                whitelist = [
                  "chrome-extension-scheme"
                  "moz-extension-scheme"
                ];
              };
              "${sidebery.addonId}".settings = builtins.fromJSON (builtins.readFile ./sidebery.json);

              # "${vimium-c.addonId}".settings = {
              #   keyMappings = [
              #     "#!no-check"
              #     "map s LinkHints.activateSelect"
              #     "map K previousTab"
              #     "map , previousTab"
              #     "map J nextTab"
              #     "map . nextTab"
              #     "map q LinkHints.activateWithQueue"
              #   ];
              #   notifyUpdate = false;
              #   searchUrl = "https://kagi.com/search?q=$s Kagi";
              # };
            };
          };

          search = {
            force = true;
            default = "Kagi";
            engines = {
              google.metaData.hidden = true;
              bing.metaData.hidden = true;
              ebay.metaData.hidden = true;
              Amazon.metaData.hidden = true;
            };
          };
          # # https://github.com/arkenfox/user.js/blob/master/user.js
          settings = {

            # PREF: notification interval (in microseconds) to avoid layout thrashing
            #  When Firefox is loading a page, it periodically reformats
            #  or "reflows" the page as it loads. The page displays new elements
            #  every 0.12 seconds by default. These redraws increase the total page load time.
            #  The default value provides good incremental display of content
            #  without causing an increase in page load time.
            #  [NOTE] Lowering the interval will increase responsiveness
            #  but also increase the total load time.
            #  [WARNING] If this value is set below 1/10 of a second, it starts
            #  to impact page load performance.
            #  [EXAMPLE] 100000 = .10s = 100 reflows/second
            #  [1] https://searchfox.org/mozilla-central/rev/c1180ea13e73eb985a49b15c0d90e977a1aa919c/modules/libpref/init/StaticPrefList.yaml#1824-1834
            #  [2] https://web.archive.org/web/20240115073722/https://dev.opera.com/articles/efficient-javascript/?page=3#reflow
            #  [3] https://web.archive.org/web/20240115073722/https://dev.opera.com/articles/efficient-javascript/?page=3#smoothspeed
            "content.notify.interval" = 100000; # (.10s); default=120000 (.12s)
            /**
              GFX **
            */
            "gfx.canvas.accelerated.cache-items" = 4096;
            "gfx.canvas.accelerated.cache-size" = 512;
            "gfx.content.skia-font-cache-size" = 20;

            /**
              DISK CACHE **
            */
            "browser.cache.disk.enable" = true;

            /**
              MEDIA CACHE **
            */
            "media.memory_cache_max_size" = 65536;
            "media.cache_readahead_limit" = 7200;
            "media.cache_resume_threshold" = 3600;

            /**
              IMAGE CACHE **
            */
            "image.mem.decode_bytes_at_a_time" = 32768;

            /**
              NETWORK **
            */
            "network.http.max-connections" = 1800;
            "network.http.max-persistent-connections-per-server" = 10;
            "network.http.max-urgent-start-excessive-connections-per-host" = 5;
            "network.http.pacing.requests.enabled" = false;
            "network.dnsCacheExpiration" = 3600;
            "network.ssl_tokens_cache_capacity" = 10240;

            /**
              SPECULATIVE LOADING **
            */
            "network.dns.disablePrefetch" = true;
            "network.dns.disablePrefetchFromHTTPS" = true;
            "network.prefetch-next" = false;
            "network.predictor.enabled" = false;
            "network.predictor.enable-prefetch" = false;

            /**
              EXPERIMENTAL **
            */
            "layout.css.grid-template-masonry-value.enabled" = true;
            "dom.enable_web_task_scheduling" = true;
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
            # PREF: Global Privacy Control (GPC) [FF118+]
            # A privacy signal that tells the websites that the user
            # doesn’t want to be tracked and doesn’t want their data to be sold.
            # Honored by many highly ranked sites [3].
            # [SETTING] Privacy & Security > Website Privacy Preferences > Tell websites not to sell or share my data
            # [TEST] https://global-privacy-control.glitch.me/
            # [1] https://bugzilla.mozilla.org/show_bug.cgi?id=1830623
            # [2] https://globalprivacycontrol.org/press-release/20201007.html
            # [3] https://github.com/arkenfox/user.js/issues/1542#issuecomment-1279823954
            # [4] https://blog.mozilla.org/netpolicy/2021/10/28/implementing-global-privacy-control/
            # [5] https://help.duckduckgo.com/duckduckgo-help-pages/privacy/gpc/
            # [6] https://brave.com/web-standards-at-brave/4-global-privacy-control/
            # [7] https://www.eff.org/gpc-privacy-badger
            # [8] https://www.eff.org/issues/do-not-track
            "privacy.globalprivacycontrol.enabled" = true;
            "browser.search.region" = "CA";

            # PREF: disable UITour backend
            # This way, there is no chance that a remote page can use it.
            "browser.uitour.enabled" = false;
            "browser.privatebrowsing.vpnpromourl" = "";
            "browser.search.countryCode" = "CA";
            "browser.search.isUS" = false;
            # PREF: disable search engine updates (e.g. OpenSearch)
            # Prevent Firefox from adding back search engines after you removed them.
            # [NOTE] This does not affect Mozilla's built-in or Web Extension search engines.
            "browser.search.update" = false;
            "browser.ctrlTab.recentlyUsedOrder" = false;
            "browser.bookmarks.showMobileBookmarks" = true;
            "browser.uidensity" = 1;
            "browser.urlbar.placeholderName" = "DuckDuckGo";
            "browser.urlbar.update1" = true;
            "distribution.searchplugins.defaultLocale" = "en-CA";
            "general.useragent.locale" = "en-CA";
            "identity.fxaccounts.account.device.name" = config.networking.hostName;
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
            # "browser.newtabpage.enabled" = false;
            # Disable Activity Stream
            # https://wiki.mozilla.org/Firefox/Activity_Stream
            # PREF: disable Firefox Home (Activity Stream) telemetry
            "browser.newtabpage.activity-stream.enabled" = false;
            "browser.newtabpage.activity-stream.feeds.telemetry" = false;
            "browser.newtabpage.activity-stream.telemetry" = false;
            # "browser.newtab.url" = "about:blank";
            # "browser.newtab.url" = "yuanwang.dev";
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
            "browser.disableResetPrompt" = true;
            # "Looks like you haven't started Firefox in a while."
            "browser.onboarding.enabled" = false; # "New to Firefox? Let's get started!" tour
            "browser.aboutConfig.showWarning" = false; # Warning when opening about:config
            "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
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
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
            "extensions.htmlaboutaddons.recommendations.enabled" = false;
            "extensions.htmlaboutaddons.discover.enabled" = false;
            "extensions.getAddons.showPane" = false; # uses Google Analytics
            "browser.discovery.enabled" = false;
            # PREF: disable Normandy/Shield [FF60+]
            # Shield is an telemetry system (including Heartbeat) that can also push and test "recipes".
            # [1] https://mozilla.github.io/normandy/
            "app.normandy.enabled" = false;
            "app.normandy.api_url" = "";
            # PREF: disable Studies
            # [SETTING] Privacy & Security>Firefox Data Collection & Use>Allow Firefox to install and run studies
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

            # PREF: disable telemetry
            # - If "unified" is false then "enabled" controls the telemetry module
            # - If "unified" is true then "enabled" only controls whether to record extended data
            # [NOTE] "toolkit.telemetry.enabled" is now LOCKED to reflect prerelease (true) or release builds (false) [2]
            # [1] https://firefox-source-docs.mozilla.org/toolkit/components/telemetry/telemetry/internals/preferences.html
            # [2] https://medium.com/georg-fritzsche/data-preference-changes-in-firefox-58-2d5df9c428b5
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
            "toolkit.telemetry.server" = "data:,";
            "toolkit.telemetry.newProfilePing.enabled" = false;
            "toolkit.telemetry.shutdownPingSender.enabled" = false;
            "toolkit.telemetry.updatePing.enabled" = false;
            "toolkit.telemetry.bhrPing.enabled" = false; # [FF57+] Background Hang Reporter
            "toolkit.telemetry.firstShutdownPing.enabled" = false;
            "toolkit.telemetry.dap_enabled" = false; # DEFAULT [FF108]

            # // PREF: disable Telemetry Coverage
            # // [1] https://blog.mozilla.org/data/2018/08/20/effectively-measuring-search-in-firefox/
            "toolkit.telemetry.coverage.opt-out" = true; # [HIDDEN PREF]
            "toolkit.coverage.opt-out" = true; # [FF64+] [HIDDEN PREF]
            "toolkit.coverage.endpoint.base" = "";
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

            /**
              ****************************************************************************
              * SECTION: DETECTION                                                        *
              *****************************************************************************
            */

            # PREF: disable Captive Portal detection
            # [1] https://www.eff.org/deeplinks/2017/08/how-captive-portals-interfere-wireless-security-and-privacy
            # [2] https://wiki.mozilla.org/Necko/CaptivePortal
            "captivedetect.canonicalURL" = "";
            "network.captive-portal-service.enabled" = false;

            # PREF: disable Network Connectivity checks
            # [WARNING] Do NOT use for mobile devices. May NOT be able to use Firefox on public wifi (hotels, coffee shops, etc).
            # [1] https://bugzilla.mozilla.org/1460537
            "network.connectivity-service.enabled" = false;

            # Disable crash reports
            "breakpad.reportURL" = "";
            "browser.tabs.crashReporting.sendReport" = false;
            "browser.crashReports.unsubmittedCheck.autoSubmit2" = false; # don't submit backlogged reports

            # Disable Form autofill
            # https://wiki.mozilla.org/Firefox/Features/Form_Autofill
            "browser.formfill.enable" = false;
            "extensions.formautofill.addresses.enabled" = false;
            "extensions.formautofill.available" = "off";
            "extensions.formautofill.creditCards.available" = false;
            "extensions.formautofill.creditCards.enabled" = false;
            "extensions.formautofill.heuristics.enabled" = false;

            # PREF: remove special permissions for certain mozilla domains [FF35+]
            # default = resource://app/defaults/permissions
            "permissions.manager.defaultsUrl" = "";
            #PREF: default permission for Location Requests
            #0=always ask (default), 1=allow, 2=block
            "permissions.default.geo" = 2;
            # shyfox
            ## Fill SVG Color
            "svg.context-properties.content.enabled" = true;
            #   ## CSS's `:has()` selector
            "layout.css.has-selector.enabled" = true;
          };
        };
      };
    };
  };
}
