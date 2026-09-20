{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.forge;
in
{
  options.modules.forge = {
    enable = lib.mkEnableOption "forge";
    enableZshIntegration = lib.mkEnableOption "forge zsh plugin and theme" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.llm-agents.forge
      ]
      ++ lib.optionals cfg.enableZshIntegration [
        pkgs.fzf
        pkgs.fd
      ];

      programs.zsh.initContent = lib.mkIf cfg.enableZshIntegration ''
        eval "$(forge zsh plugin)"
        eval "$(forge zsh theme)"
      '';
    };
  };
}
