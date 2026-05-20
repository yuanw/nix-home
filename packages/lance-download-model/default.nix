{
  lib,
  pkgs,
  python3,
  ...
}:

let
  python = python3.withPackages (ps: [ ps.huggingface-hub ]);
in

pkgs.stdenv.mkDerivation {
  pname = "lance-download-model";
  version = "0.1.0";

  src = null;

  strictDeps = true;

  nativeBuildInputs = [ python ];

  dontUnpack = true;

  buildPhase = ''
    runHook preBuild
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin

    cat > $out/bin/lance-download-model << 'BINSH'
    #!/bin/sh
    set -e
    : "''${LANCE_DATA_DIR:=$PWD}"
    : "''${HF_TOKEN:=}"

    MODELS_DIR="$LANCE_DATA_DIR/downloads"
    mkdir -p "$MODELS_DIR"

    echo "=== Lance Model Downloader ==="
    echo "Models directory: $MODELS_DIR"
    echo ""

    download_variant() {
      VARIANT="$1"
      DIR="$MODELS_DIR/$VARIANT"
      mkdir -p "$DIR"
      echo "Downloading $VARIANT ..."

      python3 -c "
    import os, sys
    from huggingface_hub import snapshot_download, hf_hub_download

    repo_id = 'bytedance-research/Lance'
    variant = '$VARIANT'
    dest = '$DIR'

    snapshot_download(
        repo_id=repo_id,
        allow_patterns=[f'{variant}/*'],
        local_dir=dest,
        local_dir_use_symlinks=False,
    )
    print(f'Downloaded {variant} to {dest}')
    "
    }

    download_variant "Lance_3B"
    download_variant "Lance_3B_Video"

    echo "Downloading shared components ..."
    python3 -c "
    from huggingface_hub import snapshot_download, hf_hub_download
    repo_id = 'bytedance-research/Lance'

    snapshot_download(
        repo_id=repo_id,
        allow_patterns=['Qwen2.5-VL-ViT/*'],
        local_dir='$MODELS_DIR',
        local_dir_use_symlinks=False,
    )

    hf_hub_download(
        repo_id=repo_id,
        filename='Wan2.2_VAE.pth',
        local_dir='$MODELS_DIR',
        local_dir_use_symlinks=False,
    )
    print('Shared components downloaded.')
    "

    echo ""
    echo "=== Download complete ==="
    echo "Models are at: $MODELS_DIR"
    ls -la "$MODELS_DIR"
    BINSH

    chmod +x $out/bin/lance-download-model
    runHook postInstall
  '';

  meta = {
    description = "Model downloader for Lance 3B multimodal AI (lightweight, huggingface-hub only)";
    homepage = "https://github.com/bytedance/Lance";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
    mainProgram = "lance-download-model";
  };
}
