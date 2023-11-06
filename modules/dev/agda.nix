{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.agda;
in {
  options.modules.dev.agda = { enable = mkEnableOption "agda"; };

  config = mkIf cfg.enable {
    # nixpkgs = {
    #   overlays = [
    #     inputs.agda.overlay
    #   ];
    # };
    home-manager.users.${config.my.username} = {
      # https://agda.readthedocs.io/en/v2.6.4/tools/package-system.html
      home.file = { "agda/defaults".text = "standard-library"; };
      home.packages = [
        (pkgs.agda.withPackages (p: [
          p.standard-library
        ]))
      ];
      programs = {
        zsh = { sessionVariables = { AGDA_DIR = "$XDG_CONFIG_HOME/agda"; }; };
      };
      # agda does not use xdg
      xdg.configFile = { "agda/defaults".text = "standard-library"; };
    };
  };
}
