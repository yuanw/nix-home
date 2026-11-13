# Firefox/LibreWolf add-on for Spookfox, in the same layout as
# nur.repos.rycee.firefox-addons packages so it can go straight into
# profiles.<name>.extensions.packages.  Not on the add-on store (upstream never
# submitted it), but the release .xpi is AMO-signed, so LibreWolf accepts it.
{
  fetchurl,
  runCommand,
}:
let
  # manifest.json -> browser_specific_settings.gecko.id
  addonId = "spookfox@bitspook.in";
  version = "0.8.1";
  mozillaExtensionDir = "share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}";
in
runCommand "spookfox-firefox-extension" { } ''
  dst="$out/${mozillaExtensionDir}"
  mkdir -p "$dst"
  install -m644 ${
    fetchurl {
      url = "https://github.com/bitspook/spookfox/releases/download/v${version}/spookfox-ff.xpi";
      sha256 = "sha256-y5vXo2K3LDSsT1eFqfyt/TVkuRMSOTeclEyMaGrQG1E=";
    }
  } "$dst/${addonId}.xpi"
''
// {
  inherit addonId version;
}
