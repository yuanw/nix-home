{
  lib,
  stdenv,
  cockpit,
  nodejs,
  python3,
  fetchFromGitHub,
  jq,
  nvidiaDriverPkg ? null,
  runCommand,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "cockpit-gpu";

  src = fetchFromGitHub {
    owner = "binbinsh";
    repo = "cockpit-gpu";
    rev = "3cb1708ca7de2565d7adc844a6e5ad64a7ad6e25";
    hash = "sha256-w8eH630TZabH1uZF6CU+v2g04m9TmdkKAY4fifKk4lE=";
  };

  # Extract version from the plugin's manifest.json via jq.
  version =
    let
      versionFile =
        runCommand "cockpit-gpu-version.txt"
          {
            nativeBuildInputs = [ jq ];
          }
          ''
            jq -r '.version' ${finalAttrs.src}/plugin/manifest.json > $out
          '';
    in
    lib.fileContents versionFile;

  # Build against the *parent* cockpit tree (provides build.js,
  # pkg/lib, patternfly, esbuild config, files.js registry).
  cockpitSrc = cockpit.src;

  nativeBuildInputs = [
    nodejs
    python3
    jq
  ];

  postPatch = ''
    # Make build.js (a perl+node script) executable and patch its shebang.
    patchShebangs "$cockpitSrc/build.js"

    # Register the plugin in cockpit's files.js so build.js picks it up.
    # This mirrors install.sh's patch_cockpit_files_js step.
    python3 "${./register-plugin.py}" "$cockpitSrc/files.js" \
        "cockpit-gpu/cockpit-gpu.jsx" "cockpit-gpu/index.html"
  '';

  buildPhase = ''
    runHook preBuild
    export NODE_ENV=production
    # Sync the plugin source into the cockpit build tree (mirrors
    # install.sh's build_dist_from_plugin_source → rsync step).
    rsync -a --delete plugin/ "$cockpitSrc/pkg/cockpit-gpu/"
    # Run the cockpit build pipeline (esbuild bundling) from the
    # cockpit source directory.
    ( cd "$cockpitSrc" && ./build.js cockpit-gpu )
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    dest="$out/share/cockpit/gpus"
    mkdir -p "$dest"

    # Copy the built dist artifacts from the cockpit build tree.
    cp -r "$cockpitSrc/dist/cockpit-gpu/." "$dest/"

    # install.sh also copies a few static files from the plugin dir
    # when they are not present in the dist output.
    for f in index.html po.js; do
      [ -f "$dest/$f" ] || cp "plugin/$f" "$dest/"
    done

    runHook postInstall
  '';

  passthru = {
    # Injected into cockpit.service PATH by the nixpkgs module's depsEnv.
    cockpitPath = lib.optional (nvidiaDriverPkg != null) nvidiaDriverPkg;
  };

  meta = {
    description = "Cockpit plugin for monitoring NVIDIA GPUs (nvidia-smi)";
    homepage = "https://github.com/binbinsh/cockpit-gpu";
    platforms = lib.platforms.linux;
    license = with lib; [ licenses.lgpl21Plus ];
  };
})
