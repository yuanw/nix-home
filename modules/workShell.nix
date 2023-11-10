{ config, lib, pkgs, packages, ... }:

with lib;
let homeDir = config.my.homeDirectory;
in {
  options.modules.workShell = { enable = mkEnableOption "workivaShell"; };

  config = mkIf config.modules.workShell.enable {

    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.kubernetes-helm
        pkgs.aws-iam-authenticator
        pkgs.clang-tools
        # pkgs.antlr4
        packages.resource-id
        pkgs.csvkit
        # pkgs.visidata
        pkgs.terraform
        pkgs.terraform-ls
        # pkgs.podman
      ];
      programs.zsh = {
        shellAliases = { bt = "bigskyTest"; };
        profileExtra = mkAfter ''

          [[ -s "${homeDir}/.wk/profile" ]] && source "${homeDir}/.wk/profile"
        '';
        initExtra = mkAfter ''
          # eval "$(pyenv init -)"
          # export PYENV_ROOT="${homeDir}/.pyenv" # needed by pipenv

          # function bigskyTest {
          #    python manage.py test $1 --http-integration --traceback -v 2
          # }

          export PATH=$PATH:$HOME/go/bin
        '';
      };

    };
  };
}
