# Firefox extension package in the same layout as nur.repos.rycee.firefox-addons.
{
  runCommand,
  browser-cli-extension,
}:
let
  addonId = "browser-cli-controller@thalheim.io";
  geckoDir = "share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}";
in
runCommand "browser-cli-firefox-extension" { } ''
  dst="$out/${geckoDir}"
  mkdir -p "$dst"
  install -m644 ${browser-cli-extension}/browser-cli-extension.xpi "$dst/${addonId}.xpi"
''
// {
  inherit addonId;
}
