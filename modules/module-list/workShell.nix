{ config, lib, pkgs, ... }:

with lib;
let
  homeDir = builtins.getEnv ("HOME");
  cfg = config.programs.workivaShell;
in {
  options.programs.workShell = { enable = mkEnableOption "workivaShell"; };

  config = mkIf config.programs.workShell.enable {
    home-manager.users.yuanwang.programs.zsh.initExtra = mkAfter ''
      eval "$(pyenv init -)"
      export GOPATH="${homeDir}/go-workspace"
      export PATH=$PATH:$GOPATH/bin
      export PYENV_ROOT="${homeDir}/.pyenv" # needed by pipenv

      function bigskyTest {
         python manage.py test $1 --http-integration --traceback -v 2
      }

      #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
      export SDKMAN_DIR="${homeDir}/.sdkman"
      [[ -s "${homeDir}/.sdkman/bin/sdkman-init.sh" ]] && source "${homeDir}/.sdkman/bin/sdkman-init.sh"
    '';
  };
}
