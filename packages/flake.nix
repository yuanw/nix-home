{
  description = "My packages";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem =
        { config, system, ... }:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            # Override mlx to 0.31.1 pre-built Metal wheel on Apple Silicon,
            # following https://github.com/jwiegley/nix-config/blob/7073e4b/overlays/30-ai-python.nix#L82
            overlays = [
              (
                _final: prev:
                prev.lib.optionalAttrs (prev.stdenv.isDarwin && prev.stdenv.isAarch64) {
                  pythonPackagesExtensions = (prev.pythonPackagesExtensions or [ ]) ++ [
                    (pfinal: pprev: {
                      mlx = pprev.mlx.overridePythonAttrs (
                        oldAttrs:
                        let
                          mlxMetalWheel = pfinal.fetchPypi {
                            pname = "mlx_metal";
                            version = "0.31.1";
                            format = "wheel";
                            dist = "py3";
                            python = "py3";
                            platform = "macosx_14_0_arm64";
                            hash = "sha256-cHQRdBMdv3/dR5y3MOBuCMNY6sO/eQXZ6ITnlgz91bg=";
                          };
                        in
                        {
                          version = "0.31.1";
                          pyproject = null;
                          format = "wheel";
                          patches = [ ];
                          postPatch = "";
                          doCheck = false;
                          # mlx-metal contents are merged in postInstall below
                          pythonRemoveDeps = [ "mlx-metal" ];
                          src = pfinal.fetchPypi {
                            pname = "mlx";
                            version = "0.31.1";
                            format = "wheel";
                            dist = "cp313";
                            python = "cp313";
                            abi = "cp313";
                            platform = "macosx_14_0_arm64";
                            hash = "sha256-mm00EPyVG9KFCP7ZwatdmQP29rsQHDpdY9QZHUmjhKE=";
                          };
                          nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ prev.unzip ];
                          postInstall = ''
                            unzip -o ${mlxMetalWheel} -d $TMPDIR/mlx-metal
                            siteDir=$out/lib/python3.13/site-packages/mlx
                            cp -r $TMPDIR/mlx-metal/mlx/lib     $siteDir/
                            cp -r $TMPDIR/mlx-metal/mlx/include  $siteDir/
                            cp -r $TMPDIR/mlx-metal/mlx/share    $siteDir/
                          '';
                        }
                      );
                    })
                  ];
                }
              )
            ];
          };
        in
        {
          overlayAttrs = config.packages;
          packages = rec {
            sketchybar-cpu-helper = pkgs.callPackage ./sketchybar-cpu-helper { };
            bandcamp-dl = pkgs.python3Packages.callPackage ./bandcamp { };
            parakeet-mlx = pkgs.python3Packages.callPackage ./parakeet-mlx.nix { };
            mlx-speech = pkgs.python3Packages.callPackage ./mlx-speech.nix { };
            mlx-speak = pkgs.callPackage ./mlx-speak.nix {
              inherit (pkgs) writers;
              inherit mlx-speech;
            };
            mlx-speak-server = pkgs.callPackage ./mlx-speak-server.nix {
              inherit (pkgs) writers;
              inherit mlx-speech;
            };
            parakeet-transcribe = pkgs.callPackage ./parakeet-transcribe.nix {
              inherit parakeet-mlx;
            };
            parakeet-mlx-server = pkgs.callPackage ./parakeet-mlx-server.nix {
              inherit (pkgs) writers;
              inherit parakeet-mlx;
            };
            parakeet-mlx-server-test = pkgs.callPackage ./parakeet-mlx-server-test.nix {
              inherit (pkgs)
                curl
                ffmpeg
                python3
                ;
            };
            claude-voice = pkgs.callPackage ./claude-voice.nix {
              inherit (pkgs) curl;
              inherit parakeet-mlx;
            };
            choose-mac = pkgs.callPackage ./choose-mac.nix { };
            sf-symbols = pkgs.callPackage ./sf_symbols.nix { };
            font-hack-nerd-font = pkgs.callPackage ./font-hack-nerd-font.nix { };
            # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/emacs-packages.nix
            hurl-mode = pkgs.callPackage ./hurl-mode.nix {
              melpaBuild = pkgs.stdenv.mkDerivation;
              inherit (pkgs) fetchFromGitHub writeText;
            };
          };
        };
      flake = { };
    };
}
