# Enterprise policies for WebGL and cookie persistence on LibreWolf.
#
# user.js / profiles.home.settings are not enough on their own: with
# browser.contentblocking.category = "strict", Firefox re-enables
# privacy.fingerprintingProtection at runtime and blocks WebGL.
# Enterprise policies override that behavior.
{
  Preferences = {
    "privacy.sanitize.sanitizeOnShutdown" = false;
    "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
    "browser.sessionstore.privacy_level" = 0;
    "webgl.disabled" = false;
    # Nix macOS builds often fail to launch the GPU helper; without this pref
    # Firefox disables WebGL entirely (FEATURE_WEBGL_NO_GPU_PROCESS).
    "webgl.allow-in-parent" = true;
    "webgl.force-enabled" = true;
    "webgl.ignore-blocklist" = true;
    "privacy.fingerprintingProtection" = false;
    # RFP also blocks WebGL; cookie persistence is handled by the sanitize prefs above.
    "privacy.resistFingerprinting" = false;
  };
}
