# The two pinned sources the native TensorFold packaging needs:
#
# - src: upstream TensorFold, exactly the container recipe's pin
#   (scripts/config.sh: TF_REPO=ashhart/TensorFold, TF_VERSION=v0.6.1,
#   "the patches and start.sh's flags are made for TensorFold v0.6.1
#   exactly (17c73e1)"). One deployment patch (patches/0002-flash-next-
#   v061.patch) carries what upstream 0.6.1 still lacks (video + many
#   images on Flash Next vision, copy drafts with compact MTP rows, the
#   TENSORFOLD_PREFILL_ROWS override, SSD read-ahead, first token before
#   the next draft, 96 MiB request bodies); patches/languages/ is only
#   for the DRAFT_LANGUAGE image and is not applied here.
# - deploySrc: yuanw's deployment repo at the rev the v0.6.1 recipe
#   (scripts/config.sh, start.sh) pins. It supplies that patch and the
#   regression tools (tools/*.py).
{
  fetchFromGitHub,
}:

{
  src = fetchFromGitHub {
    owner = "ashhart";
    repo = "TensorFold";
    rev = "17c73e189f5e6a5304cda7ea37f086f9c49b4788"; # v0.6.1
    hash = "sha256-RYVCxyEf1FEftW6UA1MMSABSgUN22h8uyc97uEfhqqo=";
  };

  deploySrc = fetchFromGitHub {
    owner = "yuanw";
    repo = "Qwen3.8-Flash-Next-Single-DGX-Spark-TensorFold";
    rev = "4c0dea8ebe93a1d445d6a716b5a38995ff6e9362"; # the v0.6.1 recipe
    hash = "sha256-YMc9wEMaq3y2rEIOnQYw0DrHER/MHX5HDhS0c2FNJkI=";
  };
}
