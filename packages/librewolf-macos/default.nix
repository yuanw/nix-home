# LibreWolf .app with distribution/policies.json baked in (macOS aarch64).
# https://github.com/Mic92/dotfiles/blob/6040591/pkgs/librewolf-macos/default.nix
{
  lib,
  stdenv,
  fetchurl,
  undmg,
  policies ? { },
  browser-cli-extension ? null,
}:
let
  srcs = lib.importJSON ./srcs.json;
in
stdenv.mkDerivation {
  pname = "librewolf";
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
    ${lib.optionalString (browser-cli-extension != null) ''
      cp ${browser-cli-extension}/browser-cli-extension.xpi         "$out/Applications/LibreWolf.app/Contents/Resources/distribution/browser-cli-extension.xpi"
    ''}
    echo '${
      builtins.toJSON { policies = policies; }
    }' > "$out/Applications/LibreWolf.app/Contents/Resources/distribution/policies.json"

    runHook postInstall
  '';

  meta = {
    description = "A privacy-focused fork of Firefox";
    homepage = "https://librewolf.net/";
    license = lib.licenses.mpl20;
    platforms = [ "aarch64-darwin" ];
    mainProgram = "librewolf";
  };
}
