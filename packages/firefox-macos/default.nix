# Firefox .app with distribution/policies.json and browser-cli XPI baked in (macOS).
# The XPI must live inside the .app — Firefox cannot read file:// URLs into /nix/store.
{
  lib,
  stdenv,
  firefox,
  browser-cli-extension,
  policies ? { },
}:
stdenv.mkDerivation {
  pname = "firefox-macos";
  version = lib.getVersion firefox;

  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/Applications"
    cp -r ${firefox}/Applications/Firefox.app "$out/Applications/"
    chmod -R u+w "$out/Applications/Firefox.app"

    dist="$out/Applications/Firefox.app/Contents/Resources/distribution"
    mkdir -p "$dist"
    cp ${browser-cli-extension}/browser-cli-extension.xpi "$dist/browser-cli-extension.xpi"
    echo '${builtins.toJSON { policies = policies; }}' > "$dist/policies.json"

    runHook postInstall
  '';

  meta = {
    description = "Firefox with enterprise policies baked in";
    homepage = "https://www.mozilla.org/firefox/";
    license = lib.licenses.mpl20;
    platforms = [
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    mainProgram = "firefox";
  };
}
