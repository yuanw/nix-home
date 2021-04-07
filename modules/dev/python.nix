{ config, lib, pkgs, localConfig, ... }:

with lib;
let cfg = config.programs.python;
in {
  options.programs.python = {
    enable = mkEnableOption "python";
    package = mkOption {
      type = types.package;
      default = pkgs.python38;
      defaultText = literalExample "pkgs.python3";
      description = ''
        python package to install.
      '';
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username}.home.packages = [
      cfg.package
      pkgs.black
      pkgs.python38Packages.pyflakes
      pkgs.python38Packages.pytest
      pkgs.python38Packages.isort
    ];
  };
}
