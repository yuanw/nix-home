# RED-SNOW-5.3-Flash 2.49bpw SAGE serving as a systemd service — native
# Nix, no container.
#
# Packages vcruz305/exllamav3 @ 7c1636f (the fork RED-SNOW's DEPLOY.md
# pins, AOT-compiled for sm_121 by packages/red-snow — replacing the
# recipe's build.sh Docker image) together with the repo's server/
# layer (packages/red-snow/server.nix) on the CUDA-enabled nixpkgs
# torch, and runs serve_glm.py directly — replacing serve.sh's
# `docker run` with one unit:
#
#   preStart  `hf download vcruz305/RED-SNOW-... --revision 071adef`
#             (~105 GB, resumable, skipped once a snapshot is cached),
#             then points stateDir/model at the snapshot dir.
#   ExecStart serve.sh's page-cache drops — hard before the load, then
#             every 2 s for at most 10 min while the loader streams the
#             weights (GB10's loader undercounts the file cache as free
#             unified memory; without the drops the full 262K Q4 cache
#             does not fit) — then exec serve_glm.py with the recipe's
#             flags: -gs 112 --cache_size 262144 --cache_quant 4 --mtp
#             --num_draft_tokens 2.
#
# The unit runs as root exactly because of those drops (serve.sh needed
# passwordless sudo for `tee /proc/sys/vm/drop_caches`); stateDir and
# the shared HF cache are root-owned either way.
#
# First start: the download (hours on a LAN uplink) + a 4-6 min load;
# "Native API listening" in the journal, then /v1 (chat, streaming,
# tool calls) on :8899. One model at a time on 128 GB unified memory:
# stop tensorfold before starting this one.
{
  config,
  pkgs,
  lib,
  ...
}:

let
  inherit (lib)
    mkIf
    mkEnableOption
    mkOption
    types
    optional
    toString
    replaceStrings
    concatStringsSep
    ;
  cfg = config.services.red-snow;

  # LD_LIBRARY_PATH for the prebuilt extension: libcuda with the
  # driver (/run/opengl-driver/lib) plus the merged redist tree —
  # several CUDA packages keep libs in separate outputs, so all
  # outputs are merged (same shape as the package's build tree).
  getAllOutputs = p: map (o: p.${o}) p.outputs;
  cudaHome = pkgs.symlinkJoin {
    name = "cuda-merged-${pkgs.cudaPackages.cudaMajorMinorVersion}";
    paths = builtins.concatMap getAllOutputs (
      with pkgs.cudaPackages;
      [
        cuda_cudart
        libcublas
        libcurand
        libcusparse
        libcusolver
      ]
    );
  };

  # python313, not the default python3 (3.14): the CUDA-enabled torch
  # lives in python313Packages (the same scope TensorFold rides on).
  # transformers >= 5.0 + jinja2 load the GLM-5.3 tokenizer and chat
  # template (the PyTorch image's own transformers could not);
  # jsonschema is protocol.py's tool-schema validator.
  #
  # The interpreter's own package scope (python313.pkgs, what
  # withPackages hands as `ps`) is NOT this overlay's python313Packages:
  # ps.exllamav3 would pick nixpkgs' upstream x86_64-only 1.5.0 build. So
  # the AOT CUDA exllamav3 rides in explicitly, exactly how the
  # tensorfold unit passes its package; transformers/jinja2/jsonschema
  # are identical in both scopes.
  rsPython = pkgs.python313.withPackages (
    ps: with ps; [
      pkgs.python313Packages.exllamav3
      transformers
      jinja2
      jsonschema
    ]
  );

  server = pkgs.redsnow-server;

  # serve.sh's exact flags, composed so a disabled --mtp collapses
  # cleanly (no dangling line continuations).
  serveArgs = concatStringsSep " " (
    [
      "-m ${cfg.stateDir}/model"
      "-gs ${toString cfg.gpuSplitGb}"
      "--cache_size ${toString cfg.context}"
      "--cache_quant ${toString cfg.cacheQuant}"
      "--num_draft_tokens ${toString cfg.mtpDraftTokens}"
      "--max-model-len ${toString cfg.context}"
      "--host ${cfg.host}"
      "--port ${toString cfg.port}"
      "--served-model-name ${cfg.servedName}"
      "--request-timeout ${toString cfg.requestTimeout}"
    ]
    ++ optional cfg.mtp "--mtp"
  );

  # serve.sh, minus docker: the cache drops (root — see the unit) and
  # exec. The dropper loop is a child of this script, so it dies with
  # the unit's cgroup, and it self-expires after 300 rounds anyway.
  serveSh = pkgs.writeShellScript "redsnow-serve" ''
    set -euo pipefail
    sync
    echo 3 > /proc/sys/vm/drop_caches
    ( for _ in $(seq 1 300); do
        sync
        echo 1 > /proc/sys/vm/drop_caches 2>/dev/null || true
        sleep 2
      done ) &
    exec ${rsPython}/bin/python \
      ${server}/share/redsnow-server/serve_glm.py ${serveArgs}
  '';

  environment = {
    HOME = cfg.stateDir;
    # Serve only from the local cache: the model loads by path, not
    # repo id. preStart overrides this for the download.
    HF_HUB_OFFLINE = "1";
    HF_HOME = cfg.hfCache;
    # libcuda (driver) + cudart for the prebuilt exllamav3_ext .so.
    LD_LIBRARY_PATH = "/run/opengl-driver/lib:${cudaHome}/lib64:${cudaHome}/lib";
  };
in
{
  options.services.red-snow = {
    enable = mkEnableOption "RED-SNOW-5.3-Flash 2.49bpw serving (native exllamav3, CUDA)";

    host = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "Bind address for the OpenAI-compatible API.";
    };

    port = mkOption {
      type = types.port;
      default = 8899;
      description = "TCP port for the OpenAI-compatible API.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the firewall for the API port.";
    };

    autoStart = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Start on boot. Off by default: the first start downloads the
        ~105 GB checkpoint, and the unit is normally managed by hand
        (one model at a time on 128 GB unified memory).
      '';
    };

    modelId = mkOption {
      type = types.str;
      default = "vcruz305/RED-SNOW-5.3-FLASH-EXL3-SAGE-2.49bpw";
      description = "HuggingFace repo ID of the RED-SNOW checkpoint.";
    };

    modelRevision = mkOption {
      type = types.str;
      default = "071adef"; # the recipe's pin
      description = "Revision of the checkpoint to download and serve.";
    };

    gpuSplitGb = mkOption {
      type = types.ints.positive;
      default = 112;
      description = ''
        Explicit model+cache split in GB (-gs). On GB10's unified memory
        auto-split stops with "Insufficient VRAM in split for model and
        cache" (it undercounts free unified memory); 112 is the value
        both Vic's MiMo Spark guide and this recipe use.
      '';
    };

    context = mkOption {
      type = types.ints.positive;
      default = 262144;
      description = "KV cache size in tokens (--cache_size, also --max-model-len).";
    };

    cacheQuant = mkOption {
      type = types.ints.positive;
      default = 4;
      description = "KV cache precision in bits (--cache_quant).";
    };

    mtp = mkOption {
      type = types.bool;
      default = true;
      description = "MTP speculative decoding (--mtp).";
    };

    mtpDraftTokens = mkOption {
      type = types.ints.positive;
      default = 2;
      description = "Draft tokens per MTP round (--num_draft_tokens).";
    };

    requestTimeout = mkOption {
      type = types.number;
      default = 7200;
      description = "Per-request server time limit in seconds.";
    };

    servedName = mkOption {
      type = types.str;
      default = "RED-SNOW-5.3-FLASH-EXL3-2.49";
      description = "Model id clients see in /v1/models and replies.";
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var/lib/redsnow";
      description = ''
        State directory: the model snapshot symlink and the service
        user's HOME.
      '';
    };

    hfCache = mkOption {
      type = types.path;
      default = "/var/lib/vllm/huggingface";
      description = ''
        HuggingFace cache holding the checkpoint (~105 GB), shared with
        the other serving recipes.
      '';
    };

    environmentFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        Environment file loaded for the service (e.g. an agenix secret
        with HF_TOKEN for the checkpoint download if the repo is gated).
      '';
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.tmpfiles.rules = [
      "d ${cfg.stateDir} 0755 root root - -"
    ];

    systemd.services.red-snow = {
      description = "RED-SNOW-5.3-Flash 2.49bpw server (native exllamav3, CUDA)";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = optional cfg.autoStart "multi-user.target";

      # The checkpoint download from the recipe (`hf download ...
      # --revision 071adef`, resumable): skipped once a snapshot is
      # cached; HF_HUB_OFFLINE=0 overrides the unit env so the first
      # run can reach the Hub (HF_TOKEN via environmentFile).
      preStart = ''
        set -eu
        modelDir="${cfg.hfCache}/hub/models--${replaceStrings [ "/" ] [ "--" ] cfg.modelId}"
        if [ -z "$(ls -d "$modelDir"/snapshots/*/ 2>/dev/null)" ]; then
          echo "downloading ${cfg.modelId} @ ${cfg.modelRevision} (~105 GB) into ${cfg.hfCache}/hub (resumes if interrupted)"
          HF_HUB_OFFLINE=0 ${pkgs.python313Packages.huggingface-hub}/bin/hf download ${cfg.modelId} --revision ${cfg.modelRevision} --cache-dir "${cfg.hfCache}/hub"
        else
          echo "checkpoint present: $modelDir"
        fi
        ln -sfn "$(ls -d "$modelDir"/snapshots/*/ | head -n 1)" "${cfg.stateDir}/model"
      '';

      inherit environment;

      serviceConfig = {
        Type = "simple";
        # No User=/Group=: the page-cache drops need root (the recipe
        # used passwordless sudo for the same writes).
        WorkingDirectory = cfg.stateDir;
        EnvironmentFile = optional (cfg.environmentFile != null) cfg.environmentFile;
        ExecStart = "${serveSh}";
        LimitMEMLOCK = "infinity"; # the container ran --ulimit memlock=-1
        # Every start loads for 4-6 min (plus the first download):
        # no start timeout.
        TimeoutStartSec = 0;
        TimeoutStopSec = 120;
        # SIGINT (not SIGTERM) so serve_glm's finally-block can drain
        # streams and close the worker cleanly.
        KillSignal = "SIGINT";
      };
    };
  };
}
