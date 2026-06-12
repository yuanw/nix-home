_final: prev: {
  # Switch to CUDA 13.2 (Blackwell/sm121 support, g++ 15 compatible)
  cudaPackages = prev.cudaPackages_13_2;

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
    })
  ];
}
