{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # The DGX Spark node builds its closure against this nixpkgs, not the
    # shared nixpkgs above: the current unstable pin can no longer build
    # linux-7.0.14-nvidia (kernel tools fail on glibc's __u128 in
    # asm/sigcontext.h — NixOS/nixpkgs#580678/#581990 fix one path, the
    # nvidia-modules rebuild hits another). This is graham33/nixos-dgx-spark's
    # pin (its flake.lock, 2026-11-14), which his Spark deployment builds from.
    # Bump/drop when unstable builds the kernel+nvidia again.
    nixpkgs-spark = {
      url = "github:NixOS/nixpkgs/58a4d239dccd";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    direnv-instant.url = "github:Mic92/direnv-instant";

    # Faster nix-collect-garbage / nix-store --optimise; its darwin module
    # replaces the hand-rolled launchd GC daemons (modules/macintosh.nix).
    #
    # Deliberately NOT `inputs.nixpkgs.follows': the module callPackages its
    # package.nix with *our* pkgs, so the two binaries come from our nixpkgs
    # whichever way this is pinned; the input's own nixpkgs-unstable only feeds
    # its checks, which we never evaluate.  Following ours would just repin
    # something we do not build.
    fast-nix-gc.url = "github:Mic92/fast-nix-gc";

    iets.url = "github:numtide/iets";

    # Project jumper (modules/terminal/h.nix): `h <name>` finds a repo by name,
    # `h <owner>/<repo>` and `h <url>` name <codeRoot>/<host>/<owner>/<repo> and
    # clone it when it is missing.  The workspace tree is laid out for it.
    h = {
      url = "github:zimbatm/h";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mics-skills.url = "github:Mic92/mics-skills";

    flake-prompt = {
      url = "gitlab:fresheyeball/flake-prompt";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Jane Street's agent skills, imported with flake-prompt's skillFromDir
    # (see modules/coding-agents/prompts). Plain source tree, not a flake.
    janestreet-skills = {
      url = "github:janestreet/agent_skills";
      flake = false;
    };

    # jwiegley's prompt collection. The caveman skill is imported the same way
    # (see modules/coding-agents/prompts); `just update claude-prompts' bumps it.
    claude-prompts = {
      url = "github:jwiegley/claude-prompts";
      flake = false;
    };

    # Single-skill and skill-collection repos, each read straight from the
    # source tree by flake-prompt's skillFromDir (modules/coding-agents/prompts).
    # `just update <name>' bumps one; no hashes to keep current.
    humanizer = {
      url = "github:blader/humanizer";
      flake = false;
    };
    i-have-adhd = {
      url = "github:ayghri/i-have-adhd";
      flake = false;
    };
    emacs-skills = {
      url = "github:xenodium/emacs-skills";
      flake = false;
    };
    # Cursor's own plugin repo. Only pstack's agents/ is read from it, as an
    # flake-prompt agent (modules/coding-agents/prompts) and as a Cursor
    # subagent under ~/.cursor/agents (modules/coding-agents/cursor).
    cursor-plugins = {
      url = "github:cursor/plugins";
      flake = false;
    };

    # Fork: adds the upstream skills to the open-code-review package.
    llm-agents.url = "github:numtide/llm-agents.nix";
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
    # Builds the Emacs config; see modules/editor/emacs/lib.nix.
    nix-emacs-home = {
      url = "github:yuanw/nix-emacs-home";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
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
    # Declarative Jellyfin config over the REST API; replaced
    # Sveske-Juice/declarative-jellyfin, which wrote system.xml and the SQLite
    # DB directly and asserted on Jellyfin versions it had not been taught
    # (that assertion is what forced the 10.11.x pin on misfit).
    #
    # Deliberately NOT `inputs.nixpkgs.follows`: the input's nixpkgs plays no
    # part in our build anyway — its module does `pkgs.callPackage ../package.nix {}`
    # with *our* pkgs, so the Node/pnpm versions that build jellarr are ours
    # either way. Following ours would only repin its checks.
    jellarr.url = "github:venkyr77/jellarr";
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
    # graham33's DGX Spark module (upstream now: PR #197, the fork's reason to
    # exist, merged 2026-11). Its nvidia kernel adds tools_128bit_types, which
    # the nixpkgs-unstable of Nov 2026 needs to build resolve_btfids on arm64
    # (glibc's __u128 in asm/sigcontext.h).
    dgx-spark.url = "github:graham33/nixos-dgx-spark";
    # Pinned to 7408b8b (2026-07-29), the last rev before upstream added the
    # enable-container-tests input as a relative path (path:./flags/...): Lix
    # refuses to lock a non-root flake whose lock file carries a path node
    # ("lock file contains mutable lock"), which fails `just update-all`.  Flip
    # back to github:nixified-ai/flake once that flag is an immutable input.
    nixified-ai = {
      url = "github:nixified-ai/flake/7408b8bca6c0043dee3b583093e8a5b79a7bda89";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    colmena.url = "github:zhaofengli/colmena";
    # Fork of we-are-mono/nix carrying the two ASK fixes this board needs --
    # libnetfilter_conntrack's 8-byte CTA_COMCERTO_FP_XFRM_HANDLE and
    # mono-gateway.ask.ipsecOffload (see hosts/mono-gateway/default.nix). Both
    # are open upstream (we-are-mono/nix#4, #5); flip this back to
    # github:we-are-mono/nix once they merge.
    #
    # nixos-26.05 branch (94c2e8f) replaces the 11071e2 pin, which existed only
    # to dodge a broken ipsec_flow_add sed: the unescaped & made the
    # short-circuit expand to invalid C, so ask-offload.nix's postPatch
    # assertion failed and 11071e2 dodged it by being shadowed (mkAfter absent),
    # which left ask.ipsecOffload = false inert. The branch escapes the &, so the
    # override now runs and the grep assertion is what proves it. It also moves
    # the fork's own nixpkgs from the EOL nixos-25.11 (23d72da, 2026-02-07) to a
    # maintained nixos-26.05 (6aefcda, 2026-09-09), rebases the cmm conntrack
    # patch onto it and fixes the initrd for the newer systemd. Kernel unchanged:
    # 6.12.49 at nxp-qoriq/linux@df24f94, so this bump fixes no kernel CVE.
    mono-nix.url = "github:yuanw/mono-gateway-nix/f6560787c95b95af9d0457d3213e6d2e38770892";
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
      # TEMPORARY: tracking the `arr` branch, which is ahead of main by the
      # misfit-torrent.org write-up (docs only -- same modules as main, so this
      # does not change any built system). Once arr merges to main, drop the
      # ?ref= again or future `nix flake update` runs keep tracking a stale
      # branch.
      url = "git+https://github.com/yuanw/nix-home-private?ref=refs/heads/arr";
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
            lib = inputs.nixpkgs.lib;
            inherit (inputs)
              flake-prompt
              janestreet-skills
              claude-prompts
              humanizer
              i-have-adhd
              emacs-skills
              cursor-plugins
              ;
          };
        in
        {
          _module.args.pkgs = import ./lib/mk-pkgs.nix {
            # one definition of the package set, in lib/mk-pkgs.nix, with this
            # flake as its only caller.  It is not exported to another repository:
            # what is measured there is that a caller outside a flake-parts
            # module of this flake cannot bind the primed input set the modules
            # read -- see the note in lib/mk-pkgs.nix.
            nixpkgs = inputs.nixpkgs;
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
            # try the transcription scripts without switching the whole system
            transcribe = pkgs.transcribe;
            install-fonts = pkgs.install-fonts;
            yt-dlp-librewolf = pkgs.yt-dlp-librewolf;
            # Exposed so `just update-rex' can reach its passthru.updateScript
            # (the pins live in packages/rex/sources.json).
            rex = pkgs.rex;
            # Same for Ladybird's dmg pin (packages/ladybird/sources.json).
            ladybird-bin = pkgs.ladybird-bin;
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
              }
            else
              { }
          );
          treefmt.imports = [ ./treefmt.nix ];
          pre-commit.settings.hooks.treefmt.enable = true;
          pre-commit.settings.package = pkgs.prek;

        };
    };
}
