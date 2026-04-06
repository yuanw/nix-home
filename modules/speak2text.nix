{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.speak2text;

  pythonWithHf = pkgs.python3.withPackages (p: [ p.huggingface-hub ]);

  parakeet-mlx-init = pkgs.writeShellApplication {
    name = "parakeet-mlx-init";
    runtimeInputs = [ pythonWithHf ];
    text = ''
      echo "Downloading mlx-community/parakeet-tdt-0.6b-v3 to HuggingFace cache..." >&2
      python3 -c "
      from huggingface_hub import snapshot_download
      path = snapshot_download('mlx-community/parakeet-tdt-0.6b-v3')
      print(f'Model cached at: {path}')
      "
    '';
  };

  cohere-transcribe-init = pkgs.writeShellApplication {
    name = "cohere-transcribe-init";
    runtimeInputs = [ pythonWithHf ];
    text = ''
      MODEL_DIR="''${COHERE_TRANSCRIBE_MODEL_DIR:-''${HOME}/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026}"
      echo "Note: CohereLabs/cohere-transcribe-03-2026 is an access-controlled model." >&2
      echo "Run 'huggingface-cli login' first if you haven't already." >&2
      echo "Downloading to ''${MODEL_DIR} ..." >&2
      huggingface-cli download CohereLabs/cohere-transcribe-03-2026 \
        --local-dir "''${MODEL_DIR}"
      echo "Copying vocab.json from Nix package..." >&2
      cp ${pkgs.cohere-transcribe}/share/cohere-transcribe/vocab.json "''${MODEL_DIR}/"
      echo "Done. Model ready at ''${MODEL_DIR}" >&2
    '';
  };

  packages = {
    whispercpp = [ pkgs.whisper-cpp ];
    parakeet-mlx = [
      pkgs.parakeet-mlx
      pkgs.parakeet-transcribe
      parakeet-mlx-init
    ];
    cohere-transcribe = [
      pkgs.cohere-transcribe
      cohere-transcribe-init
    ];
  };
in
{
  options.modules.speak2text = {
    enable = lib.mkEnableOption "speak2text";
    flavor = lib.mkOption {
      type = lib.types.enum [
        "whispercpp"
        "parakeet-mlx"
        "cohere-transcribe"
      ];
      default = "parakeet-mlx";
      description = "Speech-to-text backend. whispercpp: cross-platform CPU; parakeet-mlx: Apple Silicon MLX; cohere-transcribe: Rust + MLX (aarch64-darwin only).";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = packages.${cfg.flavor};
    };
  };
}
