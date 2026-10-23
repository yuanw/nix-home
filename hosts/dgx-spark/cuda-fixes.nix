_final: prev: {
  # No cudaPackages repin here.  This overlay used to force cudaPackages_13_2
  # "for Blackwell/sm121 support, g++ 15 compatible", which was true of an older
  # nixpkgs: at the rev the Spark builds on now nixpkgs' own default is 13.4, and
  # 13.2 is not what cache.nixos.org publishes for the CUDA 13 line at nixpkgs'
  # default capability list — keeping the repin cost cuda13.2-nccl,
  # cuda13.2-libnvshmem and cuda13.2-gdrcopy as source builds on the box.

  # Fix _cuda extensions for aarch64: cuda_compat fails on linux-sbsa (aarch64 servers)
  _cuda = prev._cuda.extend (
    _: prevAttrs: {
      extensions = prevAttrs.extensions ++ [
        (prev.lib.optionalAttrs (prev.stdenv.hostPlatform.system == "aarch64-linux") (
          _: _: { cuda_compat = null; }
        ))
      ];
    }
  );

  # Disable CUDA in OpenCV (CUDA 13 incompat)
  opencv4 = prev.opencv4.override { enableCuda = false; };

  # Unblock kornia-rs for aarch64
  python3Packages = prev.python3Packages.overrideScope (
    _pyfinal: pyprev: {
      kornia-rs = pyprev.kornia-rs.overridePythonAttrs (oldAttrs: {
        meta = oldAttrs.meta // {
          badPlatforms = [ ];
        };
      });
    }
  );

  # Python fixes for CUDA 13 (torch broken = false, test disables)
  pythonPackagesExtensions = (prev.pythonPackagesExtensions or [ ]) ++ [
    (_python-final: python-prev: {
      torch = python-prev.torch.overridePythonAttrs (oldAttrs: {
        meta = oldAttrs.meta // {
          broken = false;
        };
      });
      accelerate = python-prev.accelerate.overridePythonAttrs { doCheck = false; };
      peft = python-prev.peft.overridePythonAttrs { doCheck = false; };
      compressed-tensors = python-prev.compressed-tensors.overridePythonAttrs { doCheck = false; };
      torchaudio = python-prev.torchaudio.overridePythonAttrs { doCheck = false; };
      # bitsandbytes CUDA compilation fails on aarch64 (missing crt/host_config.h).
      # Not needed for NVFP4 inference — disable CUDA backend.
      bitsandbytes = python-prev.bitsandbytes.override { cudaSupport = false; };
    })
  ];
}
