_final: prev:
let
  # media -> text; the module that puts these two on PATH is
  # modules/speech2text/transcribe.nix.  whisper.cpp is not passed in by name:
  # `transcribe' looks transcribers up on PATH (nixpkgs calls the package
  # whisper-cpp and installs the binary as whisper-cli), and a host that wants
  # a different one names it in modules.transcribe.asrCmd -- see hosts/mist.nix.
  transcribePkgs = prev.callPackage ./transcribe.nix {
    srt2txt = ./srt2txt.awk;
  };

in
{
  installApplication =
    {
      name,
      version,
      src,
      description,
      homepage,
      ...
    }:
    with _final;
    stdenv.mkDerivation {
      name = "${name}-${version}";
      version = "${version}";
      src = src;
      buildInputs = [ _7zz ];
      sourceRoot = ".";
      phases = [
        "unpackPhase"
        "installPhase"
      ];
      unpackCmd = ''
        7zz x $src -snld
      '';
      installPhase = ''
        runHook preInstall
        mkdir -p $out/Applications
        cp -r *.app "$out/Applications/"

        mkdir -p $out/bin

        runHook postInstall
      '';

      meta = with _final.lib; {
        description = description;
        homepage = homepage;
        platforms = platforms.darwin;
      };
    };
  # https://github.com/Homebrew/homebrew-cask/blob/f144ade7bcc8884fdf2a57b114cf11e7d98b2c93/Casks/c/calibre.rb
  calibre_mac = _final.installApplication rec {
    name = "calibre";
    version = "8.4.0";
    src = prev.fetchurl {
      url = "https://download.calibre-ebook.com/${version}/calibre-${version}.dmg";
      # hash = prev.lib.fakeHash;
      hash = "sha256-NngNwMrQ0bLhDYc/Gnf/cFisAC+fMR7JQeXBjH/+m68=";
    };
    description = "e-book library management";
    homepage = "https://calibre-ebook.com/";
    # postInsall ln -s "$out/Applications/calibre.app/Contents/MacOS/ebook-convert" "$out/bin/ebook-convert"
  };

  alerter = prev.callPackage ./alerter { };
  dart = prev.callPackage ./dart.nix { };
  hosts = prev.callPackage ./hosts.nix { };
  reveal-js = prev.callPackage ./reveal-js.nix { };
  # https://github.com/NixOS/nixpkgs/pull/295747
  # yabai = prev.yabai.overrideAttrs (_finalAttrs: _previousAttrs: {
  #   version = "7.0.2";
  #   src =
  #     if prev.hostPlatform.system == "x86_64-darwin" then
  #       (
  #         prev.fetchFromGitHub {
  #           owner = "koekeishiya";
  #           repo = "yabai";
  #           rev = "v7.0.2";
  #           hash = "sha256-/MOAKsY7MlRWdvUQwHeITTeGJbCUdX7blZZAl2zXuic=";
  #         }
  #       ) else
  #       (prev.fetchzip {
  #         url = "https://github.com/koekeishiya/yabai/releases/download/v7.0.2/yabai-v7.0.2.tar.gz";
  #         hash = "sha256-FeNiJJM5vdzFT9s7N9cTjLYxKEfzZnKE9br13lkQhJo=";
  #       });
  # });
  # jdt-language-server = prev.jdt-language-server.overrideAttrs (
  #   _finalAttrs: _previousAttrs: {
  #     postPatch = ''
  #       # We store the plugins, config, and features folder in different locations
  #       # than in the original package. In addition, hard-code the path to the jdk
  #       # in the wrapper, instead of searching for it in PATH at runtime.
  #       substituteInPlace bin/jdtls.py \
  #         --replace "jdtls_base_path = Path(__file__).parent.parent" "jdtls_base_path = Path(\"$out/share/java/jdtls/\")"
  #     '';
  #   }
  # );

  # The 1Password desktop app, pinned by packages/_1password-gui/sources.json so
  # a release does not have to wait for nixpkgs to notice it -- see the note in
  # that file for why an overrideAttrs and not an override.
  _1password-gui = import ./_1password-gui prev;

  haskellPackages = prev.haskellPackages.override {
    overrides = haskellPackagesNew: _haskellPackagesOld: rec {
      ask = haskellPackagesNew.callPackage ./ask/release.nix { };
    };
  };
  agnix =
    with prev;
    rustPlatform.buildRustPackage rec {
      pname = "agnix";
      version = "0.16.5";

      src = fetchFromGitHub {
        owner = "avifenesh";
        repo = "agnix";
        tag = "v${version}";
        hash = "sha256-VUd+i1vogfLMkoJ/hzYR6besxlnKWSyH4LBYPfs1h0o=";
      };

      cargoHash = "sha256-amLDxWS4kIMeMsgAlhqECuixXd8xuYLVCzi3K9mEg1M=";

      cargoBuildFlags = [
        "--package"
        "agnix-cli"
        "--package"
        "agnix-lsp"
        "--package"
        "agnix-mcp"
      ];

      doCheck = false;

      meta = with lib; {
        description = "Linter and LSP for AI coding assistant config files (CLAUDE.md, AGENTS.md, hooks, MCP)";
        homepage = "https://github.com/avifenesh/agnix";
        license = with licenses; [
          mit
          asl20
        ];
        mainProgram = "agnix";
      };
    };
  chroma-mcp = prev.python3Packages.callPackage ./chroma-mcp.nix { };
  sketchybar-app-font = prev.callPackage ./sketchybar-app-font.nix { };
  tccutil-manage = prev.python3Packages.callPackage ./tccutil { };
  bandcamp-dl = prev.python3Packages.callPackage ./bandcamp { };
  choose-mac = prev.callPackage ./choose-mac.nix { };
  proton-vpn-cli = prev.python3Packages.callPackage ./proton-vpn-cli { };
  sf-symbols = prev.callPackage ./sf_symbols.nix { };
  font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
  # ical-buddy = prev.callPackage ./ical-buddy.nix { };
  sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };
  mermaid-cli-wrapped = prev.callPackage ./mermaid-cli-wrapped.nix { };
  cursor-agent-acp = prev.callPackage ./cursor-agent-acp.nix { };
  pi-acp = prev.callPackage ./pi-acp.nix { };
  pi-extensions = prev.callPackage ./pi-extensions { };
  parakeet-mlx = prev.python3Packages.callPackage ./parakeet-mlx.nix { };
  parakeet-transcribe = prev.callPackage ./parakeet-transcribe.nix { };
  transcribe = transcribePkgs.transcribe;
  yt-dlp-librewolf = transcribePkgs.yt-dlp-librewolf;
  parakeet-mlx-server = prev.callPackage ./parakeet-mlx-server.nix {
    inherit (prev) writers;
    parakeet-mlx = prev.python3Packages.callPackage ./parakeet-mlx.nix { };
  };
  parakeet-mlx-server-test = prev.callPackage ./parakeet-mlx-server-test.nix {
    inherit (prev)
      curl
      ffmpeg
      python3
      ;
  };

  tensorfold-zig = prev.callPackage ./tensorfold-zig { };

  ffmpeg-full = prev.ffmpeg-full.override { withWhisper = false; };

  cockpit-gpu = prev.callPackage ./cockpit-gpu.nix { };

  # HealthSave Observatory's api/worker/migrate, built from the same release
  # tarball the module fetches for Grafana.  python313Packages (not the default
  # python3) because upstream requires >=3.12,<3.14 — see the header comment.
  healthsave-observatory = prev.python313Packages.callPackage ./healthsave-observatory.nix {
    inherit (prev) fetchzip;
  };

  # ComfyUI — built with nixified-ai overlays + CUDA 13.2 fixes
  # Referenced after nixified-ai overlays are applied in dgx-spark/default.nix
  # Uses a fixed wrapper (not the nixified-ai symlinkJoin wrapper) that avoids
  # builtins.readDir on the source fetch during evaluation.
  # The nixified-ai wrapper calls:
  #   builtins.readDir (comfyui-unwrapped.src + "/models")
  # in its let-bindings, which forces Nix to realize the fetch derivation
  # during eval. This fails when colmena evaluates aarch64-linux configs on
  # aarch64-darwin (cross-architecture). The unwrapped package already wraps
  # PYTHONPATH via buildPythonApplication's postFixup, so we just join it.
  comfyui = prev.symlinkJoin {
    name = "comfyui-wrapped";
    paths = [ prev.comfyuiPackages.comfyui-unwrapped ];
    meta.mainProgram = "comfyui";
  };

  # sm121-vllm-nvfp4: SM121-optimized vLLM v0.24.0 NVFP4 KV Docker image
  # Builds from source (vLLM + FlashInfer patches) — ~60 min on DGX Spark
  sm121-vllm-nvfp4 = prev.callPackage ./sm121-vllm-nvfp4 { };

  # Performance Co-Pilot — system performance monitoring toolkit
  # Re-adds pcp removed from nixpkgs (PR #495646)
  pcp = prev.callPackage ./pcp/package.nix { };

  # Rex: terminal multiplexer with a native macOS client + Linux server.
  # Pinned by packages/rex/sources.json (snapshot prerelease channel); refresh
  # with the passthru.updateScript.  See modules/terminal-multiplexer/rex.nix.
  rex = prev.callPackage ./rex/package.nix { };

  # Ladybird browser from upstream's notarized macOS dmg, pinned in
  # packages/ladybird/sources.json (refresh: `just update-ladybird').  Named
  # -bin so nixpkgs' from-source `ladybird' stays reachable.
  # See modules/browsers/ladybird.nix.
  ladybird-bin = prev.callPackage ./ladybird/package.nix { };

}
