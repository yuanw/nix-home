{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    direnv-instant.url = "github:Mic92/direnv-instant";

    mics-skills.url = "github:Mic92/mics-skills";

    flake-prompt = {
      url = "gitlab:fresheyeball/flake-prompt";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Fork: adds the upstream skills to the open-code-review package.
    llm-agents.url = "github:yuanwang-wf/llm-agents.nix";
    # Declarative Pi configuration (programs.pi): flake-prompt skills, MCP
    # servers and extension packages. Follows this repo's nixpkgs/flake-prompt
    # and the llm-agents already used for pkgs.llm-agents.pi, so Pi stays one
    # derivation across the two flakes.
    pi-flake = {
      url = "github:yuanw/pi-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-prompt.follows = "flake-prompt";
      inputs.llm-agents.follows = "llm-agents";
      inputs.home-manager.follows = "home-manager";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    betterfox = {
      url = "github:HeitorAugustoLN/betterfox-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";
    nur.url = "github:nix-community/NUR";
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nima = {
      url = "github:yuanw/nima/set-and-vars";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mcp-servers-nix = {
      url = "github:natsukium/mcp-servers-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    declarative-jellyfin = {
      url = "github:Sveske-Juice/declarative-jellyfin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };
    impermanence.url = "github:nix-community/impermanence";
    nix-casks = {
      url = "github:atahanyorganci/nix-casks/archive";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.flake-parts.follows = "flake-parts";
    };
    nix-darwin-login-items.url = "github:yuanw/nix-darwin-login-items";
    neru = {
      url = "github:y3owk1n/neru";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    preservation = {
      url = "github:nix-community/preservation";
    };
    dgx-spark.url = "github:graham33/nixos-dgx-spark";
    # The nixpkgs graham33's own lock pins, so the Spark runs the revision his
    # CI tests against.  It also decides whether CUDA torch is a download: the
    # only prebuilt aarch64 CUDA torch anywhere is on cache.flox.dev, published
    # per nixpkgs revision, and this rev (c46e6c9) has it while the Oct-4 rev
    # this input sat on did not.  graham33.cachix.org, though it is a
    # substituter here, has none -- probed with `nix path-info --store`.
    # So after `nix flake update dgx-spark`, run `just spark-torch`: if it does
    # not print True with everything copied, the new rev has no torch and the
    # box will spend hours building one.
    nixpkgs-dgx = {
      url = "github:NixOS/nixpkgs";
      follows = "dgx-spark/nixpkgs";
    };
    nixified-ai = {
      url = "github:nixified-ai/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    colmena.url = "github:zhaofengli/colmena";
    # Fork of we-are-mono/nix carrying the two ASK fixes this board needs --
    # libnetfilter_conntrack's 8-byte CTA_COMCERTO_FP_XFRM_HANDLE and
    # mono-gateway.ask.ipsecOffload (see hosts/mono-gateway/default.nix). Both
    # are open upstream (we-are-mono/nix#4, #5); flip this back to
    # github:we-are-mono/nix once they merge.
    mono-nix.url = "github:yuanw/mono-gateway-nix";
    # Private modules live in a separate repo; see nix-home-private/README.md.
    #
    # git+https, not github:, deliberately: a github: input is fetched as a tarball
    # from the unauthenticated github.com/<owner>/<repo>/archive/<rev> endpoint, which
    # answers 404 for a private repo, and the token is read daemon-side so a CLI
    # --option netrc-file is not seen (verified: update fails with that 404; see the
    # note in nix-build-with-workiva-netrc.sh on the same trap). The git transport
    # authenticates client-side through the user's git credential helper, which works.
    # To use github: instead, put an access token where the DAEMON reads config.
    #
    # Public builds (CI) must not need it; disable it explicitly:
    #   nix develop --override-input nix-home-private path:$PWD/blank
    #   nix build .#mist --override-input nix-home-private path:$PWD/blank
    # (a bare ./blank is taken for the whole repo: a flake-less input ignores
    #  the subdir part of a URL, so the path: scheme is what actually works).
    # WK-only paths (hosts/wk01174.nix, nix run .#prefetch-work-sources, …)
    # throw from blank/; public hosts never force those imports.
    nix-home-private = {
      url = "git+https://github.com/yuanw/nix-home-private";
      flake = false;
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      imports = [
        ./devshell.nix
        ./hosts
        ./hosts/colmena.nix
        ./modules
        ./openwrt/flake-module.nix
        ./tests/flake-module.nix
        inputs.git-hooks.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.haskell-flake.flakeModule
      ];

      flake = {
        nixConfig = {
          extra-experimental-features = [ "pipe-operator" ];
          extra-substituters = [ "https://cache.numtide.com" ];
          extra-trusted-public-keys = [
            "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
          ];
        };
      };
      perSystem =
        { system, pkgs, ... }:
        let
          agentPrompts = import ./modules/coding-agents/prompts {
            inherit pkgs;
            lib = inputs.nixpkgs.lib;
          };

          # The CUDA package set that cache.flox.dev publishes a prebuilt torch
          # for: nixpkgs' own cudaPackages and nixpkgs' default
          # cudaCapabilities.  The Spark's system set pins both --
          # hosts/dgx-spark/cuda-fixes.nix repins cudaPackages to 13.2 and
          # hosts/default.nix sets cudaCapabilities = [ "12.0" "12.1" ] -- and
          # either pin alone rewrites torch's derivation hash, which is what
          # costs the multi-hour source build.  This set applies none of ours.
          # Measured at nixpkgs-dgx c46e6c9: all 93 runtime-closure paths of
          # python3Packages.torch substitute, nothing is built (3.7 GB).
          cudaPkgs = import inputs.nixpkgs-dgx {
            inherit system;
            config = {
              allowUnfree = true;
              cudaSupport = true;
            };
          };

          # A python with torch in it, not just the torch package: `nix shell`
          # on the bare torch output puts no python3 on PATH.
          torchEnv = cudaPkgs.python3.withPackages (ps: [
            ps.torch
            ps.numpy
          ]);
        in
        {
          _module.args.pkgs = import ./lib/mk-pkgs.nix {
            # one definition of the package set, in lib/mk-pkgs.nix, with this
            # flake as its only caller.  It is not exported to another repository:
            # what is measured there is that a caller outside a flake-parts
            # module of this flake cannot bind the primed input set the modules
            # read -- see the note in lib/mk-pkgs.nix.
            # aarch64-linux is the Spark alone (see hosts/default.nix), so the
            # pinned set below is the Spark's package set, not a second set for
            # some other host.
            nixpkgs = if system == "aarch64-linux" then inputs.nixpkgs-dgx else inputs.nixpkgs;
            fixesOverlay = inputs.dgx-spark.overlays.fixes;
            inherit system;
          };
          # haskellProjects.default = {
          #   projectRoot = ./packages;
          #   settings = { };
          #   # overrides = self: super: { };
          #   autoWire = [
          #     "packages"
          #     "apps"
          #   ]; # Wire all but the devShell
          #   devShell = {
          #     hlsCheck.enable = false;
          #   };
          # };

          packages = {
            llama-benchy = pkgs.llama-benchy;
            # try the transcription scripts without switching the whole system
            transcribe = pkgs.transcribe;
            yt-dlp-librewolf = pkgs.yt-dlp-librewolf;
            agent-prompts = inputs.flake-prompt.lib.mkPromptsPackage pkgs {
              prompts = agentPrompts;
              tools.pi.enable = true;
            };
          }
          // (
            if system == "aarch64-linux" then
              let
                # Rootfs ext4 image for the Mono Gateway DK (NXP LS1046A) — the
                # derivation output IS the ~2 GB .img file, so `result` is a file
                # symlink, good enough to curl/dd.
                #
                # Built from the NATIVE config. Upstream's
                # .#packages.aarch64-linux.rootfsImage is the same builder via
                # `gateway`, which forces nixpkgs.localSystem = x86_64-linux and
                # only makes sense when cross-compiling off an x86_64 host.
                #
                # The we-are-mono/nix flake pins its own nixpkgs (25.11); this is
                # intentionally NOT a follows — the mono kernel patches need their
                # own nixpkgs revision, unaffected by our nixos-unstable churn.
                mono-rootfs = inputs.mono-nix.nixosConfigurations.gateway-native.config.system.build.rootfsImage;

                # Our own layer, so LAN addressing / root keys / nix-daemon live in
                # git instead of being hand-edited on the device. extendModules keeps
                # upstream's overlay and module list intact and just appends ours, so
                # there is nothing to maintain when we bump mono-nix.
                mono-gateway-config = inputs.mono-nix.nixosConfigurations.gateway-native.extendModules {
                  modules = [ ./hosts/mono-gateway/default.nix ];
                };
              in
              {
                rootfsImage = mono-rootfs;
                mono-gateway-image = mono-rootfs; # old name, kept
                # The running system: build this and run switch-to-configuration on
                # the board. Far smaller delta than rootfsImage, since it reuses the
                # ~561 store paths already flashed.
                mono-gateway-system = mono-gateway-config.config.system.build.toplevel;

                # `just spark-torch` builds this on the Spark and smoke-tests it.
                torch-cuda = torchEnv;
              }
            else
              { }
          );
          # `nix develop .#devShells.aarch64-linux.torch-cuda` on the Spark.
          devShells =
            if system == "aarch64-linux" then
              {
                torch-cuda = cudaPkgs.mkShell {
                  packages = [ torchEnv ];
                };
              }
            else
              { };
          treefmt.imports = [ ./treefmt.nix ];
          pre-commit.settings.hooks.treefmt.enable = true;
          pre-commit.settings.package = pkgs.prek;

        };
    };
}
