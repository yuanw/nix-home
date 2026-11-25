# Pinned sources for the native Inkling 2 Mini engine (yuanw/MiaAI-Lab's
# Inkling-2-Mini-DGX-Spark-TensorFold recipe, packaged without the container).
#
# - src: TensorFold at Jürgen Schmied's Inkling 2 Mini CUDA head, PR #4441
#   (commit dc2ca8a1, on main 1587c8caee), from jschmied's fork — the recipe's
#   TF_REF. Upstream main's own Inkling 2 Mini port (ashhart#4387) is not used:
#   jschmied's review measured its CUDA path at ~5.5 tok/s against ~33 tok/s
#   for this engine (and found three load/logic bugs), so the recipe is built
#   on the faster head instead. dc2ca8a1 refuses --drafter/--vision by name;
#   the recipe patches below add the DSpark/DFlash2 drafters and vision.
# - recipeSrc: the recipe repo: patches/*.patch (0001–0018) applied in order,
#   the same set prepare.sh applies.
# - zigSrc: Zig 0.17.0 for aarch64-linux (the .zig-version pin; byte-identical
#   to the tarball packages/tensorfold-zig ships — shared via that src.nix).
# - xgrammarSrc: xgrammar v0.2.8 (commit-pinned) with its submodules
#   (3rdparty/picojson, dlpack, googletest, cpptrace). `zig build grammar`
#   compiles tf_grammar.cc over these into libtfgrammar.so, and insists the
#   checkout carries the exact pinned tag — fetchgit leaves no tags, so
#   default.nix's postPatch tags it.
# - jpegSrc/webpSrc: the image codecs the engine dlopens and refuses unless
#   the exact release: libturbojpeg.so.0 (3.1.4.1) and libwebpdecoder.so.3
#   (1.6.0), read by version string, not soname.
{
  fetchFromGitHub,
  fetchgit,
  fetchurl,
}:

{
  src = fetchFromGitHub {
    owner = "jschmied";
    repo = "TensorFold";
    rev = "dc2ca8a1785df6ee5c8489ee7cd18e29cf7b4568"; # PR #4441 head (C4), on main 1587c8caee
    hash = "sha256-y5HDt4e6M5p5BcjppQQMT+OvuCtmYqI4gx4XmFS9yaI=";
  };

  recipeSrc = fetchFromGitHub {
    owner = "yuanw";
    repo = "Inkling-2-Mini-DGX-Spark-TensorFold";
    rev = "9e3cf20b76e0140bebbbe6ed13f6e3a00e881f12"; # main: patches 0001–0018 (DFlash2 drafter by default)
    hash = "sha256-Itx/7TDsn8uo0m2x81Og7BqvI0xLpnaJpgl97pVEcoo=";
  };

  xgrammarSrc = fetchgit {
    url = "https://github.com/mlc-ai/xgrammar";
    rev = "97787376faee5ed8466cfad57c99855e4ce2f6aa"; # tag v0.2.8
    hash = "sha256-Ff/CLdSjq8ammfci+Eu9hY4433e7csf8EeGu2X9B2rI=";
    fetchSubmodules = true;
  };

  jpegSrc = fetchurl {
    url = "https://github.com/libjpeg-turbo/libjpeg-turbo/releases/download/3.1.4.1/libjpeg-turbo-3.1.4.1.tar.gz";
    hash = "sha256-7K6ACOLMmt4vLBu51ebU+3PnxDOGagVr2CmAdBVxoCI=";
  };

  webpSrc = fetchurl {
    url = "https://storage.googleapis.com/downloads.webmproject.org/releases/webp/libwebp-1.6.0.tar.gz";
    hash = "sha256-5KtwCb8GKf0RmC1MKqg5ZM8kTP+6c0fs05AZqeOMRWQ=";
  };
}
