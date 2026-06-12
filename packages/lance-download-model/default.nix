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

    PYTHON='__PYTHON_EXEC__'

    echo "=== Lance Model Downloader ==="
    echo "Models directory: $MODELS_DIR"
    echo ""

    download_variant() {
      VARIANT="$1"
      DIR="$MODELS_DIR/$VARIANT"
      mkdir -p "$DIR"
      echo "Downloading $VARIANT ..."

      "$PYTHON" -c "
    import os, shutil, sys
    from huggingface_hub import snapshot_download

    repo_id = 'bytedance-research/Lance'
    variant = '$VARIANT'
    dest = '$DIR'
    tmp = dest + '.tmp'

    # Download to a temp dir (snapshot_download preserves repo structure)
    if os.path.exists(tmp):
        shutil.rmtree(tmp)
    os.makedirs(tmp, exist_ok=True)
    snapshot_download(
        repo_id=repo_id,
        allow_patterns=[f'{variant}/**'],
        local_dir=tmp,
    )
    # Handle nested variant/ dir: files might be at tmp/variant/ or tmp/
    src = os.path.join(tmp, variant)
    if os.path.isdir(src):
        for f in os.listdir(src):
            s = os.path.join(src, f)
            d = os.path.join(dest, f)
            if os.path.islink(s):
                # Dereference symlinks (copy the target)
                target = os.readlink(s)
                if not os.path.isabs(target):
                    target = os.path.join(os.path.dirname(s), target)
                shutil.copy2(target, d)
            else:
                shutil.move(s, d)
    else:
        # Files are directly in tmp
        for f in os.listdir(tmp):
            shutil.move(os.path.join(tmp, f), os.path.join(dest, f))
    shutil.rmtree(tmp)
    print(f'Downloaded {variant} to {dest}')
    "
    }

    download_variant "Lance_3B"
    download_variant "Lance_3B_Video"

    echo "Downloading shared components ..."
    "$PYTHON" -c "
    import os, shutil
    from huggingface_hub import snapshot_download, hf_hub_download

    repo_id = 'bytedance-research/Lance'

    tmp = os.path.join('$MODELS_DIR', 'shared.tmp')
    if os.path.exists(tmp):
        shutil.rmtree(tmp)
    os.makedirs(tmp, exist_ok=True)
    snapshot_download(
        repo_id=repo_id,
        allow_patterns=['Qwen2.5-VL-ViT/**'],
        local_dir=tmp,
    )
    # Handle nested Qwen2.5-VL-ViT dir
    src = os.path.join(tmp, 'Qwen2.5-VL-ViT')
    dst = os.path.join('$MODELS_DIR', 'Qwen2.5-VL-ViT')
    if os.path.exists(dst):
        shutil.rmtree(dst)
    shutil.move(src, dst)
    shutil.rmtree(tmp)

    hf_hub_download(
        repo_id=repo_id,
        filename='Wan2.2_VAE.pth',
        local_dir='$MODELS_DIR',
    )
    print('Shared components downloaded.')
    "

    echo ""
    echo "=== Download complete ==="
    echo "Models are at: $MODELS_DIR"
    ls -la "$MODELS_DIR"
    BINSH

    substituteInPlace $out/bin/lance-download-model \
      --replace-fail '__PYTHON_EXEC__' '${python}/bin/python3'

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
