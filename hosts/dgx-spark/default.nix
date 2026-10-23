{ inputs, ... }:

{
  imports = [
    inputs.agenix.nixosModules.default
    inputs.dgx-spark.nixosModules.dgx-spark
    inputs.disko.nixosModules.disko
    ../../modules/cockpit.nix
    ../../modules/ds4.nix
    ../../modules/tensorfold.nix
    ../../modules/tensorfold-zig.nix
    ../../modules/red-snow.nix
    ../../modules/vllm.nix
    ../../modules/vllm-models.nix
    ../../modules/services/monitoring/pcp.nix
    ./disk-config.nix
    ./configuration.nix
  ];

  # graham33's dgx-spark.nix imports his vllm.nix — disable it in favor of ours
  disabledModules = [
    "${inputs.dgx-spark}/modules/vllm.nix"
    "services/misc/comfyui.nix"
  ];

  # His module pins cudaCapabilities to [ "12.0" "12.1" ] (modules/dgx-spark.nix:321)
  # so an nvcc build compiles two arches instead of nine. On this box that pin
  # costs more than it saves: it rewrites the hash of nccl, libnvshmem, ucx, ucc,
  # openmpi, pmix, prrte, hwloc and gdrcopy, none of which any cache publishes at
  # a restricted list, so all nine compile here — while all nine *are* cached at
  # nixpkgs' default list. Full system closure,
  # measured with `nix build --dry-run`: 150 derivations to build with the pin,
  # 141 without it. What still builds either way (torch 3.13, magma, triton,
  # exllamav3, tensorfold-zig) now compiles nine arches rather than two; if one of
  # those OOMs the 128 GB box — the failure mode his README warns about — restore
  # the pin instead of losing the nine. [ ] means unset (nixpkgs' own list), not
  # "no architectures"; the default list does emit sm_121 code that runs here, a
  # fp16 2048^3 GEMM was checked on the GB10.
  nixpkgs.config.cudaCapabilities = inputs.nixpkgs-dgx.lib.mkForce [ ];

  nixpkgs.overlays = [
    # CUDA + aarch64 fixes
    (import ./cuda-fixes.nix)
    # nixified-ai overlays (ComfyUI) — must come before packages overlay
    inputs.nixified-ai.overlays.comfyui
    inputs.nixified-ai.overlays.models
    inputs.nixified-ai.overlays.fetchers
    # Package overrides (flash-attn-4, comfyui)
    (import ../../packages)
  ];

  # colmena deployment configuration
  deployment = {
    targetHost = "dgx-spark.local";
    targetUser = "yuanw";
    buildOnTarget = true; # cross-architecture: aarch64-linux target
  };
}
