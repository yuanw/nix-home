{ inputs, ... }:

{
  imports = [
    inputs.agenix.nixosModules.default
    inputs.dgx-spark.nixosModules.dgx-spark
    inputs.disko.nixosModules.disko
    ../../modules/cockpit.nix
    ../../modules/ds4.nix
    ../../modules/lance.nix
    ../../modules/vllm.nix
    ../../modules/vllm-models.nix
    ../../modules/services/monitoring/pcp.nix
    ./disk-config.nix
    ./configuration.nix
  ];

  # graham33's dgx-spark.nix imports his vllm.nix — disable it in favor of ours
  disabledModules = [
    "${inputs.dgx-spark}/modules/vllm.nix"
  ];

  nixpkgs.overlays = [
    # CUDA 13.2 + aarch64 fixes
    (import ./cuda-fixes.nix)
    # nixified-ai overlays (ComfyUI) — must come before packages overlay
    inputs.nixified-ai.overlays.comfyui
    inputs.nixified-ai.overlays.models
    inputs.nixified-ai.overlays.fetchers
    # Package overrides (flash-attn-4, lance, comfyui)
    (import ../../packages)
  ];

  # colmena deployment configuration
  deployment = {
    targetHost = "dgx-spark.local";
    targetUser = "yuanw";
    buildOnTarget = true; # cross-architecture: aarch64-linux target
  };
}
