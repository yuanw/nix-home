{ pkgs, ... }:

{
  imports = [ ./user-common.nix ];
  programs.git = {
    enable = true;
    userEmail = "me@yuanwang.ca";

    signing = {
      key = "BF2ADAA2A98F45E7";
      signByDefault = true;
    };

    extraConfig = {
      github.user = "yuanw";
    };
  };
}
