# Declarative surface for agent-cat: the agentic-run runner, the wf client
# and the environment both read.
#
# Configuration is environment variables and one routing file. Neither is
# written below the line: `agentic-run --manager init` makes its own root,
# certificate and client profile, so no part of this module may generate a
# credential, and the manager is started by hand.
{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
let
  cfg = config.modules.agent-cat;
  system = pkgs.stdenv.hostPlatform.system;
  user = config.my.username;
  home = config.my.homeDirectory;
  piEnabled = config.modules.pi.enable or false;

  # wf is a Haskell package built by the Haskell machinery of its own flake,
  # so its lib/links carries the shared libraries of that other nixpkgs. A
  # second Haskell package in the same home.packages list makes buildEnv stop
  # on the conflicting subpath, and only bin of wf belongs on PATH, so it is
  # linked through on its own.
  wf = pkgs.buildEnv {
    name = "wf";
    paths = [ cfg.wfPackage ];
    pathsToLink = [
      "/bin"
    ];
  };

  # ext-pi is the Pi client that ships inside the agent-cat input: a
  # package.json with a pi manifest and a tree of .ts and .mjs files. Every
  # import it makes is one the Pi host supplies, so the tree carries no
  # dependencies and its source is the package. pi-flake normalises that into
  # a loadable Pi package, which is why no wrapper derivation lives here.
  extPi = inputs.pi-flake.lib.${system}.mkPiExtension {
    inherit pkgs;
    pname = "agent-cat-ext-pi";
    src = "${inputs.agent-cat.outPath}/ext-pi";
    runtimePackages = [ pkgs.nodejs ];
  };
in
{
  options.modules.agent-cat = {
    enable = lib.mkEnableOption ''
      agent-cat, that is the runner agentic-run and the client wf, on PATH and
      configured declaratively.

      Nothing here starts a service. The Pi extension is enabled through
      modules.pi.extensions.agent-cat, which this module sets.
    '';

    runner = lib.mkOption {
      type = lib.types.package;
      default = inputs.self.packages.${system}.agentic-run;
      description = ''
        The derivation that AGENT_CAT_RUNNER names. This is the direct
        agent-cat input, not the revision that agent-workflows pins inside wf
        for the library it links. The two are allowed to differ; the binary on
        PATH and the state it writes belong to this one, and the revision that
        wf links stays visible in the wf derivation.
      '';
    };

    wfPackage = lib.mkOption {
      type = lib.types.package;
      default = inputs.self.packages.${system}.agent-workflows;
      description = "wf and wf-taskmaster-stage.";
    };

    stateDir = lib.mkOption {
      type = lib.types.addCheck lib.types.str (p: lib.hasPrefix "/" p);
      default = "${home}/.pi/agent/agent-cat";
      description = ''
        AGENT_CAT_STATE_DIR, the store of runs, artifacts and the manager
        journal. This is also what ext-pi assumes when the variable is unset;
        it is stated here so that the location is declared and not assumed.
      '';
    };

    routing = lib.mkOption {
      type = lib.types.nullOr lib.types.lines;
      default = null;
      example = lib.literalExpression "lib.fileContents ./routing.example.yaml";
      description = ''
        The user layer of `${home}/.config/agent-cat/routing.yaml`, that is
        the engine instances, the model definitions and the profiles. When
        null the file is left alone and stays hand-managed. The file is mode
        0600 because it holds the secret references that engines resolve.
        See ./routing.example.yaml for the local model route of dgx-spark,
        which is verified by hand and not installed by this module.
      '';
    };

    emacs = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Add the wf Emacs client of agent-workflows to the Emacs load-path
          and require wf-service. See
          ../../editor/emacs/features/wf.nix, which reads this option.
        '';
      };

      dir = lib.mkOption {
        type = lib.types.str;
        default = "${inputs.agent-workflows}/emacs";
        internal = true;
        description = "The directory that holds wf.el, wf-manager.el and wf-service.el.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # ext-pi pins the Pi host to 1.0.1 in ext-pi/test/host-versions.test.ts,
    # which is a test of that repository and not a runtime gate: the extension
    # imports only what the host gives it, so 1.0.0 loads it. The assertion is
    # accepted until llm-agents supplies 1.0.1, at which point programs.pi is
    # bumped and nothing else about this module changes.
    modules.pi.extensions.agent-cat = {
      enable = lib.mkDefault true;
      package = extPi;
    };

    home-manager.users.${user} = {
      home.packages = [
        cfg.runner
        wf
      ];

      # A routing file is written only when it is declared, and then owned by
      # Home Manager. The manager credential is never generated here.
      home.file = lib.optionalAttrs (cfg.routing != null) {
        ".config/agent-cat/routing.yaml" = {
          text = cfg.routing;
          mode = "0600";
        };
      };

      programs.pi.environment.variables = lib.optionalAttrs piEnabled {
        AGENT_CAT_RUNNER = lib.getExe cfg.runner;
        AGENT_CAT_STATE_DIR = cfg.stateDir;
      };
    };
  };
}
