{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.python;
in
{
  options.modules.dev.python = {
    enable = mkEnableOption "python";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username}.home.packages = [
      pkgs.micromamba
      (pkgs.python3.withPackages (
        ps: with ps; [
          pip
          #ipython
          black
          pandas
          numpy
          isort
          #jupyter
          sortedcontainers
          setuptools
          pylint
          matplotlib
          # poetry
          pytest
          pyflakes
          # pylsp-mypy
          virtualenv
          virtualenvwrapper
          opentype-feature-freezer
          fonttools
          #litellm
          #tokenizers
        ]
      ))
    ];
  };
}
