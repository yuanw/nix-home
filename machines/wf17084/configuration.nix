{ config, lib, pkgs, ... }:

{
  imports = lib.attrValues (import ../../modules) ++ [
    ../../modules/workShell.nix
  ];
  networking.hostName = "wf17084";
  #services.yabai.enableScriptingAddition = false;
  #
  programs.workShell.enable = true;

  home-manager.users.yuanwang.programs.git = {
    userEmail = "yuan.wang@workiva.com";

    signing = {
      key = "19AD3F6B1A5BF3BF";
      signByDefault = true;
    };

    extraConfig = { github.user = "yuanwang-wf"; };
  };
}
