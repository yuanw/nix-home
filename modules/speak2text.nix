{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.speak2text;
  packages = {
    whispercpp = [ pkgs.whisper-cpp ];
    parakeet-mlx = [
      pkgs.parakeet-mlx
      pkgs.parakeet-transcribe
    ];
    cohere-transcribe = [ pkgs.cohere-transcribe ];
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
