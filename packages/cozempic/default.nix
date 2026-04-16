{
  buildPythonApplication,
  fetchFromGitHub,
  setuptools,
  lib,
  python,
}:
buildPythonApplication rec {
  pname = "cozempic";
  version = "1.7.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "Ruya-AI";
    repo = "cozempic";
    tag = "v${version}";
    hash = "sha256-qGEoYX4cFuI4FhIQps2uSqXl7k09TFQO4TionmPO29M=";
  };

  build-system = [ setuptools ];

  # The guard daemon forks using `sys.executable -m cozempic.cli`
  # which bypasses the Nix wrapper's site.addsitedir() injection.
  # Setting PYTHONPATH ensures the forked process can find the module.
  makeWrapperArgs = [
    "--set"
    "PYTHONPATH"
    "${builtins.placeholder "out"}/${python.sitePackages}"
  ];

  doCheck = false;

  meta = {
    description = "Context cleaning for Claude Code — prune bloated sessions, protect Agent Teams from context loss";
    homepage = "https://github.com/Ruya-AI/cozempic";
    license = lib.licenses.mit;
    mainProgram = "cozempic";
  };
}
