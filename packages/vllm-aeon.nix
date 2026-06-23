# vLLM v0.23.0 built from source for DGX Spark (aarch64-linux, sm_121a)
#
# Clean build — does NOT inherit nixpkgs v0.16 patches.
# AEON/D Flash patches to be added after baseline compiles.
#
# Build on DGX Spark:
#   nix build --impure --expr '(import <nixpkgs> { config = { allowUnfree = true; cudaSupport = true; }; overlays = [(import ./hosts/dgx-spark/cuda-fixes.nix)]; }).python3Packages.callPackage ./packages/vllm-aeon.nix {}'

{
  lib,
  stdenv,
  python,
  buildPythonPackage,
  fetchFromGitHub,
  symlinkJoin,
  autoAddDriverRunpath,
  which,
  cmake,
  grpcio-tools,
  jinja2,
  ninja,
  packaging,
  setuptools,
  setuptools-scm,
  aioprometheus,
  anthropic,
  bitsandbytes,
  blake3,
  cachetools,
  cbor2,
  compressed-tensors,
  depyf,
  einops,
  fastapi,
  gguf,
  grpcio,
  grpcio-reflection,
  ijson,
  importlib-metadata,
  llguidance,
  lm-format-enforcer,
  mcp,
  mistral-common,
  model-hosting-container-standards,
  msgspec,
  numba,
  numpy,
  openai,
  openai-harmony,
  opencv-python-headless,
  opentelemetry-api,
  opentelemetry-exporter-otlp,
  opentelemetry-sdk,
  outlines,
  pandas,
  partial-json-parser,
  prometheus-fastapi-instrumentator,
  py-cpuinfo,
  pyarrow,
  pybase64,
  pydantic,
  python-json-logger,
  python-multipart,
  pyzmq,
  ray,
  sentencepiece,
  setproctitle,
  tiktoken,
  tokenizers,
  torch,
  torchaudio,
  torchvision,
  transformers,
  uvicorn,
  xformers,
  xgrammar,
  psutil,
  py-libnuma,
  cupy,
  flashinfer,
  nvidia-ml-py,
  cudaSupport ? torch.cudaSupport,
  cudaPackages ? { },
}:

let
  inherit (lib) optionals;

  version = "0.23.0";

  # CUTLASS v4.4.2 for v0.23
  cutlass = fetchFromGitHub {
    name = "cutlass-source";
    owner = "NVIDIA";
    repo = "cutlass";
    tag = "v4.4.2";
    hash = "sha256-0q9Ad0Z6E/rO2PdM4uQc8H0E0qs9uKc3reHepiHhjEc=";
  };

  # CUTLASS v3.9.0 for FlashMLA
  cutlass-flashmla = fetchFromGitHub {
    owner = "NVIDIA";
    repo = "cutlass";
    rev = "147f5673d0c1c3dcf66f78d677fd647e4a020219";
    hash = "sha256-dHQto08IwTDOIuFUp9jwm1MWkFi8v2YJ/UESrLuG71g=";
  };

  flashmla = stdenv.mkDerivation {
    pname = "flashmla";
    version = "1.0.0";
    src = fetchFromGitHub {
      name = "FlashMLA-source";
      owner = "vllm-project";
      repo = "FlashMLA";
      rev = "a6ec2ba7bd0a7dff98b3f4d3e6b52b159c48d78b";
      hash = "sha256-Oj37H0swZdxaprpaHq0XfOCagc0ypYKpS8e6JzqcDQg=";
    };
    dontConfigure = true;
    buildPhase = ''
      rm -rf csrc/cutlass
      ln -sf ${cutlass-flashmla} csrc/cutlass
    '';
    installPhase = "cp -rva . $out";
  };

  triton-kernels = fetchFromGitHub {
    owner = "triton-lang";
    repo = "triton";
    tag = "v3.5.1";
    hash = "sha256-dyNRtS1qtU8C/iAf0Udt/1VgtKGSvng1+r2BtvT9RB4=";
  };

  qutlass = fetchFromGitHub {
    name = "qutlass-source";
    owner = "IST-DASLab";
    repo = "qutlass";
    rev = "830d2c4537c7396e14a02a46fbddd18b5d107c65";
    hash = "sha256-aG4qd0vlwP+8gudfvHwhtXCFmBOJKQQTvcwahpEqC84=";
  };

  vllm-flash-attn-src = stdenv.mkDerivation {
    pname = "vllm-flash-attn";
    version = "2.7.2.post1";
    src = fetchFromGitHub {
      name = "flash-attention-source";
      owner = "vllm-project";
      repo = "flash-attention";
      rev = "dd62dac706b1cf7895bd99b18c6cb7e7e117ee25";
      hash = "sha256-y6gIgP6a4U0UGzSxP0vjgIzqXoRSdyJei8FYEC6ITNk=";
    };
    dontConfigure = true;
    buildPhase = ''
      rm -rf csrc/cutlass
      ln -sf ${cutlass} csrc/cutlass
    '';
    installPhase = "cp -rva . $out";
  };

  gpuTargetString =
    if cudaSupport then
      "12.1a" # DGX Spark GB10 Blackwell
    else
      "native";

  mergedCudaLibraries = with cudaPackages; [
    cuda_cudart
    cuda_cccl
    libcurand
    libcusparse
    libcusolver
    cuda_nvtx
    cuda_nvrtc
    libcublas
  ];

  getAllOutputs = p: [
    (lib.getBin p)
    (lib.getLib p)
    (lib.getDev p)
  ];

in
buildPythonPackage {
  pname = "vllm-aeon";
  inherit version;
  pyproject = true;

  src = fetchFromGitHub {
    owner = "vllm-project";
    repo = "vllm";
    tag = "v${version}";
    hash = "sha256-9mxu2jLchoKmRzD71enPomVJuP5LjbUtQqLMdP5k+Qw=";
  };

  postPatch = ''
        # Remove vendored pynvml
        rm -f vllm/third_party/pynvml.py
        substituteInPlace tests/utils.py \
          --replace-fail \
            "from vllm.third_party.pynvml import" \
            "from pynvml import" || true
        substituteInPlace vllm/utils/import_utils.py \
          --replace-fail \
            "import vllm.third_party.pynvml as pynvml" \
            "import pynvml" || true

        # Relax version pins
        substituteInPlace pyproject.toml \
          --replace-fail "torch ==" "torch >=" \
          --replace-fail "setuptools>=77.0.3,<81.0.0" "setuptools" \
          --replace-fail "grpcio-tools==1.78.0" "grpcio" || true

        # Allow our Python version
        substituteInPlace CMakeLists.txt \
          --replace-fail \
            'set(PYTHON_SUPPORTED_VERSIONS' \
            'set(PYTHON_SUPPORTED_VERSIONS "${lib.versions.majorMinor python.version}"' || true

        # Skip Rust frontend — conditional feature, not needed for inference
        substituteInPlace pyproject.toml \
          --replace-fail '"setuptools-rust>=1.9.0",' '"setuptools>=77.0.3",' || true

        # setup.py imports setuptools_rust unconditionally — make it optional
        substituteInPlace setup.py \
          --replace-fail \
            'from setuptools_rust import Binding, RustExtension' \
            'try: from setuptools_rust import Binding, RustExtension
    except ImportError: Binding = RustExtension = None' || true
        substituteInPlace setup.py \
          --replace-fail \
            'from setuptools_rust.build import build_rust' \
            'try: from setuptools_rust.build import build_rust
    except ImportError: build_rust = None' || true
        # Guard Rust extension registration for when setuptools_rust is absent
        substituteInPlace setup.py \
          --replace-fail \
            'cmdclass["build_rust"] = precompiled_build_rust' \
            'if build_rust is not None: cmdclass["build_rust"] = precompiled_build_rust' || true
        substituteInPlace setup.py \
          --replace-fail \
            'rust_extensions = [' \
            'if RustExtension is not None: rust_extensions = [
    else: rust_extensions = []
    _' || true
  '';

  nativeBuildInputs = [
    which
  ]
  ++ optionals cudaSupport [
    cudaPackages.cuda_nvcc
    autoAddDriverRunpath
  ];

  build-system = [
    cmake
    grpcio-tools
    jinja2
    ninja
    packaging
    setuptools
    setuptools-scm
    torch
  ];

  buildInputs = optionals cudaSupport (
    mergedCudaLibraries
    ++ (with cudaPackages; [
      nccl
      cudnn
      libcufile
    ])
  );

  dependencies = [
    aioprometheus
    anthropic
    bitsandbytes
    blake3
    cachetools
    cbor2
    compressed-tensors
    depyf
    einops
    fastapi
    gguf
    grpcio
    grpcio-reflection
    ijson
    importlib-metadata
    llguidance
    lm-format-enforcer
    mcp
    mistral-common
    model-hosting-container-standards
    msgspec
    numba
    numpy
    openai
    openai-harmony
    opencv-python-headless
    opentelemetry-api
    opentelemetry-exporter-otlp
    opentelemetry-sdk
    outlines
    pandas
    partial-json-parser
    prometheus-fastapi-instrumentator
    py-cpuinfo
    pyarrow
    pybase64
    pydantic
    python-json-logger
    python-multipart
    pyzmq
    ray
    sentencepiece
    setproctitle
    tiktoken
    tokenizers
    torch
    torchaudio
    torchvision
    transformers
    uvicorn
    xformers
    xgrammar
  ]
  ++ uvicorn.optional-dependencies.standard
  ++ aioprometheus.optional-dependencies.starlette
  ++ optionals stdenv.targetPlatform.isLinux [
    psutil
    py-libnuma
  ]
  ++ optionals cudaSupport [
    cupy
    flashinfer
    nvidia-ml-py
  ];

  dontUseCmakeConfigure = true;

  cmakeFlags = [
    (lib.cmakeFeature "FETCHCONTENT_SOURCE_DIR_CUTLASS" "${lib.getDev cutlass}")
    (lib.cmakeFeature "FLASH_MLA_SRC_DIR" "${lib.getDev flashmla}")
    (lib.cmakeFeature "VLLM_FLASH_ATTN_SRC_DIR" "${lib.getDev vllm-flash-attn-src}")
    (lib.cmakeFeature "QUTLASS_SRC_DIR" "${lib.getDev qutlass}")
    (lib.cmakeFeature "TORCH_CUDA_ARCH_LIST" gpuTargetString)
    (lib.cmakeFeature "CUDA_TOOLKIT_ROOT_DIR" "${symlinkJoin {
      name = "cuda-merged-${cudaPackages.cudaMajorMinorVersion}";
      paths = builtins.concatMap getAllOutputs mergedCudaLibraries;
    }}")
    (lib.cmakeFeature "CAFFE2_USE_CUDNN" "ON")
    (lib.cmakeFeature "CAFFE2_USE_CUFILE" "ON")
    (lib.cmakeFeature "CUTLASS_ENABLE_CUBLAS" "ON")
    (lib.cmakeFeature "ENABLE_NVFP4_SM100" "OFF")
  ];

  env = {
    VLLM_TARGET_DEVICE = "cuda";
    CUDA_HOME = "${lib.getDev cudaPackages.cuda_nvcc}";
    TRITON_KERNELS_SRC_DIR = "${lib.getDev triton-kernels}/python/triton_kernels/triton_kernels";
    VLLM_REQUIRE_RUST_FRONTEND = "0";
  };

  preConfigure = ''
    export MAX_JOBS=''${MAX_JOBS:-8}
  '';

  pythonRelaxDeps = true;
  pythonImportsCheck = [ "vllm" ];

  # No tests — they take hours and are flaky on aarch64
  doCheck = false;

  meta = with lib; {
    description = "vLLM v0.23.0 for DGX Spark (sm_121a, aarch64-linux)";
    homepage = "https://github.com/vllm-project/vllm";
    license = licenses.asl20;
    platforms = platforms.linux;
    broken = false;
    knownVulnerabilities = [ ];
  };
}
