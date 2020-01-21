{ pkgs, ... }:

{
  imports = [ ./user-common.nix ];
  programs.git = {
    enable = true;
    userEmail = "yuan.wang@workiva.com";
    extraConfig = {
      github.user           = "yuanwang-wf";
    };
  };
}
