{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.zellij;
  zellijCmd = getExe pkgs.zellij;
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
          settings = {
            themes = "catppuccin-mocha";
          };
        };

        # https://github.com/zellij-org/zellij/issues/1933
        zsh.initExtra = mkOrder 199 ''
          eval "$(${zellijCmd} setup --generate-completion zsh | grep "^function")"
        '';

      };
    };
  };
}
