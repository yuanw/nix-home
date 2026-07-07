{
  lib,
  fetchFromGitHub,
  buildPythonApplication,
  hatchling,
  hatch-vcs,
  pythonRelaxDepsHook,
  openai,
  tokenizers,
  transformers,
  tabulate,
  numpy,
  requests,
  aiohttp,
  pydantic,
}:

buildPythonApplication rec {
  pname = "llama-benchy";
  version = "0.4.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "eugr";
    repo = "llama-benchy";
    rev = "v${version}";
    hash = "sha256-IdNjg7mDZgk9eU6V3Yczj28WlhHCtpsYlSVx4Yknz5E=";
  };

  nativeBuildInputs = [ pythonRelaxDepsHook ];

  build-system = [
    hatchling
    hatch-vcs
  ];

  dependencies = [
    openai
    tokenizers
    transformers
    tabulate
    numpy
    requests
    aiohttp
    pydantic
  ];

  # asyncio is a Python stdlib module since 3.4; the PyPI "asyncio" is a
  # deprecated backport.  llama-benchy requires >=3.10 so we drop it.
  pythonRemoveDeps = [ "asyncio" ];

  # hatch-vcs needs a git checkout to determine the version; with
  # fetchFromGitHub there's no .git, so we pretend.
  SETUPTOOLS_SCM_PRETEND_VERSION = version;

  doCheck = false; # tests require network access (mock server has imports that fail)

  pythonImportsCheck = [ "llama_benchy" ];

  meta = {
    description = "llama-bench style benchmarking tool for all OpenAI-compatible LLM endpoints";
    homepage = "https://github.com/eugr/llama-benchy";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ ];
    mainProgram = "llama-benchy";
  };
}
