# The two pinned sources the native RED-SNOW packaging needs:
#
# - src: vcruz305's exllamav3 fork at the commit RED-SNOW's DEPLOY.md
#   pins (build.sh: EXL3_REF=7c1636f, "the commit RED-SNOW's DEPLOY.md
#   pins"). Unlike TensorFold, this engine precompiles its CUDA
#   extension (setup.py CUDAExtension) instead of JIT-compiling at
#   first start, so the fork builds AOT for sm_121 in the Nix sandbox.
# - deploySrc: Weschera's RED-SNOW deployment repo at the benchmarked
#   rev (Spark-Bench 85.1); supplies the four server/*.py files (the
#   OpenAI /v1 server adapted from Vic's MiMo recipe: stdlib
#   ThreadingHTTPServer + the GLM-5.3 chat format / tool parser).
{
  fetchFromGitHub,
}:

{
  src = fetchFromGitHub {
    owner = "vcruz305";
    repo = "exllamav3";
    rev = "7c1636fead6b6d802dc3ebf0e7ef02c4e76e9c2f"; # RED-SNOW's engine pin
    hash = "sha256-GDhh+eHdpjNYmqnDNhR0fdhYk1sbLNSnUAMiTbHl7/Q=";
  };

  deploySrc = fetchFromGitHub {
    owner = "Weschera";
    repo = "RED-SNOW-5.3-Flash-2.49bpw-1x-DGX-Spark";
    rev = "b96fac226d9689c9e574f9b1b491c01d5340030d"; # the Spark-Bench 85.1 recipe
    hash = "sha256-HRUutqZpwOXg7XkKR7uRVEGwzp2JTc2No3sho4TFhmY=";
  };
}
