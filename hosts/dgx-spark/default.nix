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
