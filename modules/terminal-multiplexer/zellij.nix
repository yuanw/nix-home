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
      home.packages = [
        (pkgs.writeShellScriptBin "zellij-session" (builtins.readFile ./zellij-session.sh))
        (pkgs.writeShellScriptBin "zellij-wrapper" (builtins.readFile ./zellij-wrapper.sh))
      ];
      programs = {
        zellij = {
          enable = true;
          enableZshIntegration = true;
          settings = {
            themes = "catppuccin-mocha";
          };
        };

        # https://github.om/zellij-org/zellij/issues/1933
        zsh.initExtra = mkOrder 199 ''
          eval "$(${zellijCmd} setup --generate-completion zsh | grep "^function")"
        '';

      };
    };
  };
}
