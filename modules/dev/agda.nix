{ config, inputs, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.agda;
in {
  options.modules.dev.agda = { enable = mkEnableOption "agda"; };

  config = mkIf cfg.enable {
    nixpkgs = {
      overlays = [
        inputs.agda.overlay
      ];
    };
    home-manager.users.${config.my.username} = {
      home.packages = [
        (pkgs.agda.withPackages (p: [
          (p.standard-library.overrideAttrs (_oldAttrs: {
            version = "1.7.4";
            src = pkgs.fetchFromGitHub {
              repo = "agda-stdlib";
              owner = "agda";
              rev = "e0879db86e16c81b3c018ff46ba799f875dcc539";
              sha256 = "XNWvlI3zfMs9nf0q1sT9rsisBGZOz+/aRmek3ot++k4=";
            };
          }))
        ]))
      ];
      programs = {
        zsh = { sessionVariables = { AGDA_DIR = "$XDG_CONFIG_HOME/agda"; }; };
      };
      # agda does not use xdg
      file = { "agda/defaults".text = "standard-library"; };
      xdg.configFile = { "agda/defaults".text = "standard-library"; };
    };
  };
}
