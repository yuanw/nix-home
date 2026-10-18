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
    rev = "763f39fc8c2dd327563a5f6f9094ce19c652d4ea"; # main: 0001…0013 (0011 vision NVFP4, 0012 engine NVFP4, 0013 tune)
    hash = "sha256-rY0Rdp3yyjU9IqNjLJuoUJWghc98hVLT6LHnjJqyJ90=";
  };

  zigSrc = fetchurl {
    url = "https://ziglang.org/download/0.17.0/zig-aarch64-linux-0.17.0.tar.xz";
    hash = "sha256-no0RZh1K471XcCo4MngeI60VHd5XmOFqXM1QP2UjT/g=";
  };
}
