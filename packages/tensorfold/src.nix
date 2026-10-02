# The two pinned sources the native TensorFold packaging needs:
#
# - src: upstream TensorFold, exactly the container recipe's pin
#   (scripts/config.sh: TF_REPO=ashhart/TensorFold, TF_VERSION=v0.3.6.3).
#   The nine deployment patches are made for this exact release.
# - deploySrc: yuanw's deployment repo, the same rev the retired podman
#   unit cloned into /var/lib/qwen38-tensorfold/repo. It supplies the
#   site-packages patches (patches/*.patch) and the regression tools
#   (tools/*.py).
{
  fetchFromGitHub,
}:

{
  src = fetchFromGitHub {
    owner = "ashhart";
    repo = "TensorFold";
    rev = "v0.3.6.3";
    hash = "sha256-5OXnI+ajpXobLJC4vn/4bq6saFwejuVSoWP2AM6mqUI=";
  };

  deploySrc = fetchFromGitHub {
    owner = "yuanw";
    repo = "Qwen3.8-Flash-Next-Single-DGX-Spark-TensorFold";
    rev = "a3aa89835022c55ca8e55008c37785954834e04f";
    hash = "sha256-upiScG4RoX6Ff4v/nSJwQEOBiXv04Z409KMUAXopgs0=";
  };
}
