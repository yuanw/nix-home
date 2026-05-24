{ inputs, ... }:

{
  imports = [
    inputs.dgx-spark.nixosModules.dgx-spark
    inputs.disko.nixosModules.disko
    ../../modules/ds4.nix
    ../../modules/lance.nix
    ./disk-config.nix
    ./configuration.nix
  ];

  nixpkgs.overlays = [
    # ── CUDA 13.2 overlay ──────────────────────────────────────────
    # DGX Spark (GB10 Blackwell) requires CUDA 13.x for proper sm121 support.
    # CUDA 12.9 has g++ 15 incompatibility and lacks full Blackwell optimization.
    # CUDA 13.2 supports g++ 15 (max CUDA_GCC_VERSIONS['13.0'] = 16.0).
    # flash-attn-4's cu13 extra provides Blackwell-optimized CuTeDSL kernels.
    (_final: prev: {
      # Switch the entire CUDA stack to 13.2
      cudaPackages = prev.cudaPackages_13_2;
    })

    # ── Package overrides ─────────────────────────────────────────
    (import ../../packages)

    # ── Python package fixes for CUDA 13 ──────────────────────────
    (_final: prev: {
      pythonPackagesExtensions = (prev.pythonPackagesExtensions or [ ]) ++ [
        (_python-final: python-prev: {
          # torch is marked broken for CUDA 13 in nixpkgs; unbreak it.
          torch = python-prev.torch.overridePythonAttrs (oldAttrs: {
            meta = oldAttrs.meta // {
              broken = false;
            };
          });

          # Disable tests that fail on aarch64 (cpuinfo init failure in nix sandbox)
          accelerate = python-prev.accelerate.overridePythonAttrs { doCheck = false; };
          peft = python-prev.peft.overridePythonAttrs { doCheck = false; };
          compressed-tensors = python-prev.compressed-tensors.overridePythonAttrs { doCheck = false; };
        })
      ];
    })
  ];

  # colmena deployment configuration
  deployment = {
    targetHost = "dgx-spark.local";
    targetUser = "yuanw";
    buildOnTarget = true; # cross-architecture: aarch64-linux target
  };
}
