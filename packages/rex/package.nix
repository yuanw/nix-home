{
  lib,
  stdenv,
  stdenvNoCC,
  fetchurl,
  fetchzip,
  _7zz,
  makeBinaryWrapper,
  installShellFiles,
  patchelf,
  glibc,
  pam,
  writeShellApplication,
  coreutils,
  curl,
  jq,
  nix,
  python3,
}:
let
  inherit (stdenvNoCC.hostPlatform) isDarwin isLinux system;
  pins = lib.importJSON ./sources.json;
  serverVersion = pins.server.version;
  serverRev = pins.server.rev;
  appVersion = pins.app.version;
  appBuild = pins.app.build;
  linuxArtifacts = {
    x86_64-linux = "rex-linux-amd64.tar.gz";
    aarch64-linux = "rex-linux-arm64.tar.gz";
  };
  sources = {
    aarch64-darwin = {
      url = "https://blob.superlogical.com/public/rex-app/snapshot-arm64/Rex-${appVersion}-${appBuild}.dmg";
    };
  }
  // lib.mapAttrs (_: filename: {
    url = "https://blob.superlogical.com/download/rex/snapshot/${serverRev}/${filename}";
  }) linuxArtifacts;
  # The binary archives omit terminfo and the SSH guard shipped by upstream Nix
  support = fetchzip {
    url = "https://pkgs.superlogical.com/rex/nix/snapshot/${serverVersion}.tar.gz";
    hash = pins.server.supportHash;
  };
in
# nix run .#rex -- ... exposes the CLI on both platforms
# macOS also installs Rex Beta.app and rex-app, Linux is server-only upstream
# Native builds also install Bash, Fish, and Zsh completions
# Snapshot is upstream's rolling prerelease channel for Rex Beta
# Run passthru.updateScript (`just update-rex`) from the repo root to refresh
# every snapshot pin
stdenvNoCC.mkDerivation {
  pname = "rex";
  version = if isDarwin then "${appVersion}-${appBuild}" else serverVersion;

  strictDeps = true;
  src = fetchurl (sources.${system} // { hash = pins.hashes.${system}; });
  sourceRoot = ".";
  nativeBuildInputs = [
    installShellFiles
  ]
  ++ lib.optionals isDarwin [
    _7zz
    makeBinaryWrapper
  ]
  ++ lib.optional isLinux patchelf;

  dontConfigure = true;
  dontBuild = true;
  # Preserve the signed macOS bundle and Linux's explicit dlopen search path
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    ${
      if isDarwin then
        ''
          mkdir -p "$out/Applications"
          cp -R "Rex Beta/Rex Beta.app" "$out/Applications/"
          app="$out/Applications/Rex Beta.app/Contents"
          makeWrapper "$app/Helpers/Rex Server.rex/Contents/MacOS/rex" "$out/bin/rex"
          makeWrapper "$app/MacOS/Rex" "$out/bin/rex-app"
        ''
      else
        ''
          install -m755 rex "$out/bin/rex"
          # patchelf 0.19 corrupts this Go binary if the interpreter is set first
          # PAM is loaded by name, so retain it even though it is not DT_NEEDED
          patchelf --set-rpath "${
            lib.makeLibraryPath [
              glibc
              pam
            ]
          }" "$out/bin/rex"
          patchelf --set-interpreter "$(cat ${stdenv.cc}/nix-support/dynamic-linker)" "$out/bin/rex"
        ''
    }

    for entry in x/xterm-rex 78/xterm-rex r/rex 72/rex; do
      install -Dm644 "${support}/xterm-rex.bin" "$out/share/terminfo/$entry"
    done
    install -Dm644 "${support}/50-rex.conf" "$out/share/rex/ssh_config.d/50-rex.conf"

    ${lib.optionalString (stdenvNoCC.buildPlatform.canExecute stdenvNoCC.hostPlatform) ''
      installShellCompletion --cmd rex \
        --bash <("$out/bin/rex" completion bash) \
        --fish <("$out/bin/rex" completion fish) \
        --zsh <("$out/bin/rex" completion zsh)
    ''}

    runHook postInstall
  '';

  doInstallCheck = true;

  installCheckPhase = ''
    runHook preInstallCheck
    "$out/bin/rex" version
    "$out/bin/rex" --help
    runHook postInstallCheck
  '';

  passthru = {
    inherit serverVersion appBuild support;
    updateScript = writeShellApplication {
      name = "update-rex";
      runtimeInputs = [
        coreutils
        curl
        jq
        nix
        python3
      ];
      text = ''
        file=packages/rex/sources.json
        release=$(curl -fsSL https://blob.superlogical.com/api/releases/rex/snapshot)
        version=$(jq -er '.version' <<< "$release")
        rev=$(jq -er '.commit' <<< "$release")
        app=$(curl -fsSL https://blob.superlogical.com/public/rex-app/snapshot-arm64/appcast.xml |
        python3 -c '
        import json
        import sys
        import xml.etree.ElementTree as ET

        item = ET.parse(sys.stdin).find("channel/item")
        ns = "{http://www.andymatuschak.org/xml-namespaces/sparkle}"
        json.dump({
        "version": item.findtext(ns + "shortVersionString"),
        "build": item.findtext(ns + "version"),
        "url": item.find("enclosure").attrib["url"],
        }, sys.stdout)
        ' | jq -e 'select(.version and .build and .url)')
        app_url=$(jq -er '.url' <<< "$app")
        app_hash=$(nix --extra-experimental-features nix-command store prefetch-file --json "$app_url" \
          | jq -er '.hash')
        support_hash=$(nix --extra-experimental-features nix-command store prefetch-file --json --unpack \
          "https://pkgs.superlogical.com/rex/nix/snapshot/$version.tar.gz" | jq -er '.hash')
        hashes=$(jq -n --arg hash "$app_hash" '{"aarch64-darwin": $hash}')

        ${lib.concatMapStringsSep "\n" (target: ''
          url=$(jq -er --arg filename ${lib.escapeShellArg linuxArtifacts.${target}} \
            '.artifacts[] | select(.filename == $filename) | .downloadUrl' <<< "$release")
          hash=$(nix --extra-experimental-features nix-command store prefetch-file --json "$url" \
            | jq -er '.hash')
          hashes=$(jq --arg system ${lib.escapeShellArg target} --arg hash "$hash" \
            '. + {($system): $hash}' <<< "$hashes")
        '') (builtins.attrNames linuxArtifacts)}

        # Publish one complete pin set only after every prefetch succeeds
        staged=$(mktemp "$file.XXXXXX")
        trap 'rm -f "$staged"' EXIT
        jq -n --arg version "$version" --arg rev "$rev" \
          --arg support_hash "$support_hash" --argjson app "$app" \
          --argjson hashes "$hashes" '{
          server: {version: $version, rev: $rev, supportHash: $support_hash},
          app: ($app | del(.url)),
          hashes: $hashes
        }' > "$staged"
        chmod --reference="$file" "$staged"
        mv "$staged" "$file"
      '';
    };
  };

  meta = {
    description = "Terminal multiplexer with a native macOS client and a Linux server";
    homepage = "https://www.superlogical.com/rex";
    license = lib.licenses.unfree;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    maintainers = [ lib.maintainers._4evy ];
    platforms = builtins.attrNames sources;
    mainProgram = "rex";
  };
}
