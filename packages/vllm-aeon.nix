# vLLM v0.23.0 built from source for DGX Spark (aarch64-linux, sm_121a)
#
# Rust frontend enabled — uses cargo vendoring.
# AEON/D Flash patches to be added after baseline compiles.
#
# Build on DGX Spark via flake overlay.

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
  setuptools-rust,
  rustPlatform,
  cargo,
  rustc,
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

  # Source
  vllmSrc = fetchFromGitHub {
    owner = "vllm-project";
    repo = "vllm";
    tag = "v${version}";
    hash = "sha256-9mxu2jLchoKmRzD71enPomVJuP5LjbUtQqLMdP5k+Qw=";
  };

  # Rust dependencies — pre-fetched via fetchCargoVendor
  cargoDeps = rustPlatform.fetchCargoVendor {
    src = vllmSrc;
    sourceRoot = "${vllmSrc.name}/rust";
    hash = "sha256-mE87Pu0W4rrhjxuSdg2yzITdie7PEd0DVmfiagkH7bg=";
  };

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

  # vllm-flash-attn for v0.23 (Hopper/Blackwell kernels)
  # Uses CUTLASS v4.2.1 (not v4.4.2) — the flash-attn revision expects older API
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
      ln -sf ${
        fetchFromGitHub {
          owner = "NVIDIA";
          repo = "cutlass";
          rev = "62750a2b75c802660e4894434dc55e839f322277";
          hash = "sha256-eL64JYiFAqrn6OJDzijpMR4DbEB4El/8yT7++iCZBlE=";
        }
      } csrc/cutlass
    '';
    installPhase = "cp -rva . $out";
  };

  # DeepGEMM for v0.23
  deepgemm = fetchFromGitHub {
    name = "deepgemm-source";
    owner = "deepseek-ai";
    repo = "DeepGEMM";
    rev = "891d57b4db1071624b5c8fa0d1e51cb317fa709f";
    hash = "sha256-sQM8SFkcDJmzyvKl1nv+nkwWaHvvo7mOGyNot2oduJg=";
    fetchSubmodules = true;
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

  src = vllmSrc;

  # ─── Rust support ───
  inherit cargoDeps;
  cargoRoot = "rust";

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
  '';

  nativeBuildInputs = [
    which
    rustPlatform.cargoSetupHook
    cargo
    rustc
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
    setuptools-rust
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

  # cmakeFlags are passed via CMAKE_ARGS env var (v0.23 setup.py convention)

  env = {
    VLLM_TARGET_DEVICE = "cuda";
    CUDA_HOME = "${lib.getDev cudaPackages.cuda_nvcc}";
    TRITON_KERNELS_SRC_DIR = "${lib.getDev triton-kernels}/python/triton_kernels/triton_kernels";
    VLLM_REQUIRE_RUST_FRONTEND = "1";
    # v0.23 reads CMAKE_ARGS env var and appends to cmake command
    CMAKE_ARGS = toString [
      "-DVLLM_CUTLASS_SRC_DIR=${lib.getDev cutlass}"
      "-DFLASH_MLA_SRC_DIR=${lib.getDev flashmla}"
      "-DQUTLASS_SRC_DIR=${lib.getDev qutlass}"
      "-DDEEPGEMM_SRC_DIR=${lib.getDev deepgemm}"
      "-DVLLM_FLASH_ATTN_SRC_DIR=${lib.getDev vllm-flash-attn-src}"
      "-DTORCH_CUDA_ARCH_LIST=${gpuTargetString}"
      "-DCUDA_TOOLKIT_ROOT_DIR=${
        symlinkJoin {
          name = "cuda-merged-${cudaPackages.cudaMajorMinorVersion}";
          paths = builtins.concatMap getAllOutputs mergedCudaLibraries;
        }
      }"
      "-DCAFFE2_USE_CUDNN=ON"
      "-DCAFFE2_USE_CUFILE=ON"
      "-DCUTLASS_ENABLE_CUBLAS=ON"
      "-DENABLE_NVFP4_SM100=OFF"
    ];
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
