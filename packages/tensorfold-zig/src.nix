# Pinned sources for the native TensorFold Zig engine (branch zig-flashnext).
#
# - src: upstream TensorFold at the branch the MiaAI-Lab recipe pins. Pristine
#   it is Metal-only; the recipe patches (0001…0009) add the CUDA family
#   registry + the Flash Next CUDA engine and the AOT kernel spec.
# - recipeSrc: the MiaAI-Lab single-Spark recipe: patches/*.patch.
# - zigSrc: the official Zig 0.17.0 toolchain (the .zig-version pin). The
#   repo's pinned nixpkgs tops out at zig_0_16, and the registry's zig_0_17
#   is from a newer nixpkgs, so fetch the release tarball exactly as the
#   recipe's Dockerfile does.
{
  fetchFromGitHub,
  fetchurl,
}:

{
  src = fetchFromGitHub {
    owner = "ashhart";
    repo = "TensorFold";
    rev = "db281878ddb836fd0df510d8771ecb7e0fe47d26"; # zig-flashnext
    hash = "sha256-WVxvEmZl8+vPYbfKAMkEY3RUzz3cuqfRIFnD+R5nhqw=";
  };

  recipeSrc = fetchFromGitHub {
    owner = "MiaAI-Lab";
    repo = "Qwen3.8-Flash-Next-Single-DGX-Spark-TensorFold";
    rev = "4e1239ddb9d9a4cc3c748e771ae4725335c46a95"; # main: 0001…0009 + 0010-system-memory-cache
    hash = "sha256-U2URsdeH4PktOcl0PpNyqFPKPwA9kwBKgRtevE9+t1w=";
  };

  zigSrc = fetchurl {
    url = "https://ziglang.org/download/0.17.0/zig-aarch64-linux-0.17.0.tar.xz";
    hash = "sha256-no0RZh1K471XcCo4MngeI60VHd5XmOFqXM1QP2UjT/g=";
  };
}
