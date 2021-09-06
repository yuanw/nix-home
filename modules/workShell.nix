{ config, lib, pkgs, localConfig, ... }:

with lib;
let homeDir = localConfig.homeDirectory;
in {
  options.programs.workShell = { enable = mkEnableOption "workivaShell"; };

  config = mkIf config.programs.workShell.enable {
    home-manager.users.${localConfig.username} = {
      home.packages = [
        pkgs.kubernetes-helm
        # pkgs.aws-iam-authenticator
        pkgs.clang-tools
        pkgs.antlr4
        pkgs.csvkit
      ];
      programs.zsh = {
        shellAliases = { bt = "bigskyTest"; };
        initExtra = mkAfter ''
          export PATH=$PATH:$HOME/go/bin
          # eval "$(pyenv init -)"
          # export PYENV_ROOT="${homeDir}/.pyenv" # needed by pipenv

          function bigskyTest {
             python manage.py test $1 --http-integration --traceback -v 2
          }

          [[ -s "${homeDir}/.wk/profile" ]] && source "${homeDir}/.wk/profile"


          #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
          export SDKMAN_DIR="${homeDir}/.sdkman"
          [[ -s "${homeDir}/.sdkman/bin/sdkman-init.sh" ]] && source "${homeDir}/.sdkman/bin/sdkman-init.sh"
        '';
      };
    };
  };
}
