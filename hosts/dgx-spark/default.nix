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

  # Keep graham33's cudaCapabilities pin (modules/dgx-spark.nix:321), so an
  # nvcc build compiles sm_120/sm_121 instead of nixpkgs' ten-arch default.
  # The default list does not build on CUDA 13.4 once magma is in the closure:
  # ptxas segfaults (signal 11) on magmablas/zgeqr2_batched_fused_reg_tall.cu
  # and zgbtrf_batched_sliding_window_sm.cu at 4 workers, and at 1 worker the
  # 3492-object build takes hours. The pin is not free -- it rewrites the hash of
  # nccl, libnvshmem, ucx, ucc, openmpi, pmix, prrte, hwloc and gdrcopy, which
  # no cache publishes at a restricted list (150 derivations for the closure,
  # vs 141 at the default list) -- but it is what builds.
  nixpkgs.config.cudaCapabilities = inputs.nixpkgs-dgx.lib.mkForce [
    "12.0"
    "12.1"
  ];

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

  # Distrust the eMudhra roots (CVE-2026-111943). nixpkgs hardcodes these in
  # nixos/modules/security/ca.nix, but only on unstable/26.05; nixpkgs-dgx
  # (c46e6c9) predates that, so the names go here. At cacert 3.126 this drops
  # the system bundle from 121 to 118 blocks. Bumping nixpkgs-dgx instead would
  # rebuild the CUDA closure; this changes only cacert + its p11kit output, and
  # both are already on cache.nixos.org (the 4-name list matches newer nixpkgs).
  security.pki.caCertificateBlacklist = [
    "Comodo AAA Services root"
    "emSign ECC Root CA - G3"
    "emSign Root CA - C1"
    "emSign ECC Root CA - C3"
  ];

  # colmena deployment configuration
  deployment = {
    targetHost = "dgx-spark.local";
    targetUser = "yuanw";
    buildOnTarget = true; # cross-architecture: aarch64-linux target
  };
}
