{ pkgs, ... }:

{
  imports = [ ./user-common.nix ];
  programs.git = {
    enable = true;
    userEmail = "yuan.wang@workiva.com";

    signing = {
      key = "19AD3F6B1A5BF3BF";
      signByDefault = true;
    };

    extraConfig = {
      github.user           = "yuanwang-wf";
    };
  };
}
