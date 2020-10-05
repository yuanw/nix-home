{ config, lib, pkgs, ... }:

{
  imports = lib.attrValues (import ../../modules);
  networking.hostName = "wf17084";
  services.yabai.enableScriptingAddition = false;

  home-manager.users.yuanwang.programs.git = {
    userEmail = "yuan.wang@workiva.com";

    signing = {
      key = "19AD3F6B1A5BF3BF";
      signByDefault = true;
    };

    extraConfig = { github.user = "yuanwang-wf"; };
  };
}
