{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.python;
  localConfig = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "wf17084";
    gpgKey = "19AD3F6B1A5BF3BF";
    homeDirectory = "/Users/yuanwang";
  };
in {
  options.programs.python = { enable = mkEnableOption "python"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username}.home.packages = [
      (pkgs.python38.withPackages (ps:
        with ps; [
          pip
          ipython
          black
          isort
          setuptools
          pylint
          matplotlib
          nose
          pytest
          pyflakes
          pylsp-mypy
          virtualenv
          virtualenvwrapper
        ]))
    ] ++ [ pkgs.poetry ];
  };
}
