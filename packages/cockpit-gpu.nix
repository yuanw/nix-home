{
  lib,
  stdenv,
  cockpit,
  nodejs,
  python3,
  fetchFromGitHub,
  nvidiaDriverPkg ? null,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "cockpit-gpu";
  version = "main"; # pinned to commit; manifest.json uses CalVer-ish tags

  src = fetchFromGitHub {
    owner = "binbinsh";
    repo = "cockpit-gpu";
    rev = "3cb1708ca7de2565d7adc844a6e5ad64a7ad6e25";
    sha256 = "sha256-w8eH630TZabH1uZF6CU+v2g04m9TmdkKAY4fifKk4lE="; # fill after first build
  };

  # Plugin source lives under ./plugin in the repo; flatten it.
  sourceRoot = "${finalAttrs.src.name}/plugin";

  # Build against the *parent* cockpit tree (provides build.js,
  # pkg/lib, patternfly, esbuild config, files.js registry).
  cockpitSrc = cockpit.src;

  buildInputs = [
    nodejs
    python3
  ];

  postPatch = ''
    # Drop the plugin source into the cockpit build tree, mirroring
    # install.sh's build_dist_from_plugin_source().
    pluginDir="$NIX_BUILD_TOP/cockpit-build/pkg/cockpit-gpu"
    mkdir -p "$(dirname "$pluginDir")"
    cp -r "$PWD" "$pluginDir"

    cp -r "$cockpitSrc" "$NIX_BUILD_TOP/cockpit-build"   # tmp copy
    cb="$NIX_BUILD_TOP/cockpit-build"
    chmod -R u+w "$cb"

    # Register the plugin in cockpit's files.js so build.js picks it up
    # (same effect as install.sh patch_cockpit_files_js).
    python3 "${./register-plugin.py}" "$cb/files.js" \
        "cockpit-gpu/cockpit-gpu.jsx" "cockpit-gpu/index.html"

    patchShebangs "$cb/build.js"
  '';

  buildPhase = ''
    runHook preBuild
    cb="$NIX_BUILD_TOP/cockpit-build"
    ( cd "$cb" && NODE_ENV=production ./build.js cockpit-gpu )
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    cb="$NIX_BUILD_TOP/cockpit-build"
    dest="$out/share/cockpit/gpus"
    mkdir -p "$(dirname "$dest")"
    cp -r "$cb/dist/cockpit-gpu" "$dest"
    # Copy static assets that install.sh copies even when absent from dist
    [ -f "$dest/index.html" ] || cp "$sourceRoot/index.html" "$dest/"
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
