{ config, lib, ... }:

with lib;
let
  cfg = config.modules.zellij;
  zellijCmd = getExe config.programs.zellij.package;
in
{
  options.modules.zellij = {
    enable = mkEnableOption "zellij";
  };
  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      programs = {
        zellij = {
          enable = true;
          enableZshIntegration = true;
        };

        zsh.initExtra = mkOrder 199 ''
          eval "$(${zellijCmd} setup --generate-completion zsh)"
        '';

      };
    };
  };
}
