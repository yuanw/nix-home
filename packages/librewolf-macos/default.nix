# macOS LibreWolf from upstream Codeberg DMG with enterprise policies.
# Update: python3 packages/librewolf-macos/update.py
{
  lib,
  stdenv,
  fetchurl,
  undmg,
  policies ? { },
}:
let
  srcs = lib.importJSON ./srcs.json;
in
stdenv.mkDerivation {
  pname = "librewolf-configured";
  inherit (srcs) version;

  src = fetchurl {
    inherit (srcs) url hash;
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/Applications"
    cp -r LibreWolf.app "$out/Applications/"

    mkdir -p "$out/Applications/LibreWolf.app/Contents/Resources/distribution"
    echo '${
      builtins.toJSON { inherit policies; }
    }' > "$out/Applications/LibreWolf.app/Contents/Resources/distribution/policies.json"

    runHook postInstall
  '';

  meta = {
    description = "LibreWolf macOS app from upstream DMG with enterprise policies";
    homepage = "https://librewolf.net/";
    license = lib.licenses.mpl20;
    platforms = lib.platforms.darwin;
    mainProgram = "librewolf";
  };
}
