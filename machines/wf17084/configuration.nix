let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in { config, lib, ... }: {
  imports = lib.attrValues (import ../../modules);
  networking.hostName = "wf17084";
  #services.yabai.enableScriptingAddition = false;
  #
  programs.workShell.enable = true;
  programs.dart.enable = true;

  home-manager.users.yuanwang.programs.git = {
    userEmail = "yuan.wang@workiva.com";

    signing = {
      key = "19AD3F6B1A5BF3BF";
      signByDefault = true;
    };

    extraConfig = { github.user = "yuanwang-wf"; };
  };
}
