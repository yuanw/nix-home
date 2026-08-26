# macOS LibreWolf from upstream DMG (nix-casks) with enterprise policies.
# https://github.com/Mic92/dotfiles/blob/b5cfe872ef997444cab57776389f7e54570f1cd3/pkgs/librewolf-macos/default.nix
{
  stdenv,
  librewolf,
  policies ? { },
}:
stdenv.mkDerivation {
  pname = "${librewolf.pname}-configured";
  inherit (librewolf) version;

  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -R ${librewolf}/Applications $out/
    chmod -R u+w $out
    mkdir -p $out/Applications/LibreWolf.app/Contents/Resources/distribution
    echo '${
      builtins.toJSON { inherit policies; }
    }' > "$out/Applications/LibreWolf.app/Contents/Resources/distribution/policies.json"
    runHook postInstall
  '';

  meta = librewolf.meta // {
    description = "LibreWolf macOS app from upstream DMG with enterprise policies";
  };
}
