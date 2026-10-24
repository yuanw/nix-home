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
    # dgx-spark never imports myModules.common and carries its own substituter
    # list, so the LAN cache consumer has to be imported here
    # (docs/harmonia-build-hub.org Phase 2, finding 3).
    ../../modules/services/cache/consumer.nix
    ./disk-config.nix
    ./configuration.nix
  ];

  # graham33's dgx-spark.nix imports his vllm.nix — disable it in favor of ours
  disabledModules = [
    "${inputs.dgx-spark}/modules/vllm.nix"
    "services/misc/comfyui.nix"
  ];

  nixpkgs.overlays = [
    # CUDA 13.2 + aarch64 fixes
    (import ./cuda-fixes.nix)
    # nixified-ai overlays (ComfyUI) — must come before packages overlay
    inputs.nixified-ai.overlays.comfyui
    inputs.nixified-ai.overlays.models
    inputs.nixified-ai.overlays.fetchers
    # Package overrides (flash-attn-4, comfyui)
    (import ../../packages)
  ];

  # Distrust the eMudhra roots (CVE-2026-111943). nixpkgs hardcodes these in
  # nixos/modules/security/ca.nix, but only on unstable/26.05; the pinned
  # nixpkgs-dgx predates that, so the names go here. At cacert 3.126 this drops
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
