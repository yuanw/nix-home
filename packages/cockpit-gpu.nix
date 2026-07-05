{
  lib,
  stdenv,
  cockpit,
  nodejs,
  python3,
  fetchFromGitHub,
  jq,
  nvidiaDriverPkg ? null,
}:

stdenv.mkDerivation (_finalAttrs: {
  pname = "cockpit-gpu";

  src = fetchFromGitHub {
    owner = "binbinsh";
    repo = "cockpit-gpu";
    rev = "3cb1708ca7de2565d7adc844a6e5ad64a7ad6e25";
    hash = "sha256-w8eH630TZabH1uZF6CU+v2g04m9TmdkKAY4fifKk4lE=";
  };

  # Version hardcoded to avoid eager derivation realization during eval.
  # (lib.fileContents would force the derivation to be built at eval time,
  # which fails when colmena evaluates aarch64-linux on aarch64-darwin.)
  version = "0.0.0";

  nativeBuildInputs = [
    nodejs
    python3
    jq
  ];

  buildPhase = ''
    runHook preBuild

    # The cockpit source is in the Nix store which is mounted read-only on the
    # target system. Copy it to a writable working directory, then modify it.
    workingCockpit="$TMPDIR/cockpit-build"
    cp -R ${cockpit.src} "$workingCockpit"
    chmod -R u+w "$workingCockpit"

    # Copy pkg/lib from the Cockpit source (build templates, boot script generation).
    # This mirrors what cockpit-machines does in its postPatch.
    mkdir -p "$workingCockpit/pkg"
    cp -r ${cockpit.src}/pkg/lib "$workingCockpit/pkg/lib"

    # Register the plugin in cockpit's files.js so build.js picks it up.
    python3 "${./register-plugin.py}" "$workingCockpit/files.js" \
        "cockpit-gpu/cockpit-gpu.jsx" "cockpit-gpu/index.html"

    export NODE_ENV=production
    # Sync the plugin source into the cockpit build tree.
    cp -a --remove-destination plugin/ "$workingCockpit/pkg/cockpit-gpu/"
    # Run the cockpit build pipeline from the working directory.
    ( cd "$workingCockpit" && node build.js cockpit-gpu )
    runHook postBuild
  '';

  installPhase = ''
        runHook preInstall
        dest="$out/share/cockpit/gpus"
        mkdir -p "$dest"

        # Copy the built dist artifacts from the working directory.
        cp -r "$TMPDIR/cockpit-build/dist/cockpit-gpu/." "$dest/"

        # install.sh also copies a few static files from the plugin dir
        # when they are not present in the dist output.
        for f in index.html po.js; do
          [ -f "$dest/$f" ] || cp "plugin/$f" "$dest/"
        done

        # The dist build sometimes omits cockpit-gpu-boot.js (which the plugin's
        # index.html references). Create a minimal boot script that loads the
        # main bundle via script tag injection (avoids ESM import issues with
        # Cockpit's .js.gz transparent serving).
        if [ ! -f "$dest/cockpit-gpu-boot.js" ]; then
          cat > "$dest/cockpit-gpu-boot.js" << 'EOF'
    (function() {
      var s = document.createElement('script');
      s.src = 'cockpit-gpu.js';
      document.head.appendChild(s);
    })();
    EOF
        fi

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
