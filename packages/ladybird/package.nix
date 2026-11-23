{
  lib,
  stdenvNoCC,
  fetchurl,
  _7zz,
  makeBinaryWrapper,
  writeShellApplication,
  curl,
  jq,
  nix,
}:
let
  pins = lib.importJSON ./sources.json;
  version = pins.version;
  # Upstream's release manifest: every published release with the SHA-256 of
  # each artifact.  This is what `passthru.updateScript' reads.
  manifestUrl = "https://download.ladybird.org/releases.json";
  artifact = "ladybird-${version}-macOS-arm64.dmg";
in
# The vendor dmg holds a Developer ID-signed, notarized bundle whose dylibs are
# all @rpath-relative, so nothing is patched and the signature stays valid --
# hence dontFixup.  The only thing added is the CLI wrapper.
# Upstream's Sparkle self-update cannot write into the store, so bumps come from
# `just update-ladybird' (see the justfile).
# nixpkgs' own `ladybird' builds from source (skia, Qt, Rust); this is the
# prebuilt bundle, kept under its own name so that one stays reachable.
stdenvNoCC.mkDerivation {
  pname = "ladybird-bin";
  inherit version;

  strictDeps = true;
  src = fetchurl {
    url = "https://download.ladybird.org/macos/${version}/${artifact}";
    hash = pins.sha256;
  };

  sourceRoot = ".";
  nativeBuildInputs = [
    _7zz
    makeBinaryWrapper
  ];
  # -snld: the volume ships an `Applications -> /Applications` symlink that 7zz
  # would otherwise extract pointing into the build directory.
  unpackCmd = ''7zz x "$src" -snld'';

  dontConfigure = true;
  dontBuild = true;
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/Applications" "$out/bin"
    cp -R Ladybird.app "$out/Applications/"
    # a symlink would lose the bundle path, and Qt then misses Contents/PlugIns
    makeWrapper "$out/Applications/Ladybird.app/Contents/MacOS/Ladybird" "$out/bin/ladybird"

    runHook postInstall
  '';

  # Covers dyld and the Qt cocoa platform plugin, both resolved from the bundle.
  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    grep -q "Version ${version}" <("$out/bin/ladybird" --version)
    runHook postInstallCheck
  '';

  passthru = {
    inherit manifestUrl;
    updateScript = writeShellApplication {
      name = "update-ladybird";
      runtimeInputs = [
        curl
        jq
        nix
      ];
      text = ''
        file=packages/ladybird/sources.json
        # Fails (and so leaves the pins alone) if the newest release has no
        # arm64 macOS disk image.
        latest=$(curl -fsSL ${manifestUrl} | jq -e '
          .releases[0] as $release
          | $release.downloads[]
          | select(.platform == "macOS" and .architecture == "arm64")
          | {version: $release.version, sha256: .sha256}')
        version=$(jq -er '.version' <<< "$latest")
        sha256=$(nix --extra-experimental-features nix-command hash convert \
          --to sri --hash-algo sha256 "$(jq -er '.sha256' <<< "$latest")")

        staged=$(mktemp "$file.XXXXXX")
        trap 'rm -f "$staged"' EXIT
        jq -n --arg version "$version" --arg sha256 "$sha256" \
          '{version: $version, sha256: $sha256}' > "$staged"
        chmod --reference="$file" "$staged"
        mv "$staged" "$file"
      '';
    };
  };

  meta = {
    description = "Independent web browser using the SerenityOS LibWeb engine (upstream macOS disk image)";
    homepage = "https://ladybird.org";
    license = lib.licenses.bsd2;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    platforms = [ "aarch64-darwin" ];
    mainProgram = "ladybird";
  };
}
