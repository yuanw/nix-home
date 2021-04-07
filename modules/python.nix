{ config, lib, pkgs, localConfig, ... }:

with lib;
let cfg = config.programs.python;
in {
  options.programs.python = { enable = mkEnableOption "python"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username}.home.packages = [
      (pkgs.python37.withPackages (ps:
        with ps; [
          pip
          ipython
          black
          isort
          setuptools
          pylint
          #poetry
          pytest
          pyflakes
        ]))
    ];
  };
}
