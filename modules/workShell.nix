{ config, lib, ... }:

with lib;
let homeDir = config.my.homeDirectory;
in {
  options.programs.workShell = { enable = mkEnableOption "workivaShell"; };

  config = mkIf config.programs.workShell.enable {
    home-manager.users.${config.my.username}.programs.zsh = {
      shellAliases = { bt = "bigskyTest"; };
      initExtra = mkAfter ''
        eval "$(pyenv init -)"
        export PYENV_ROOT="${homeDir}/.pyenv" # needed by pipenv

        function bigskyTest {
           python manage.py test $1 --http-integration --traceback -v 2
        }

        #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
        export SDKMAN_DIR="${homeDir}/.sdkman"
        [[ -s "${homeDir}/.sdkman/bin/sdkman-init.sh" ]] && source "${homeDir}/.sdkman/bin/sdkman-init.sh"
      '';
    };
  };
}
