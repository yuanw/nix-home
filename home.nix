{ pkgs, ... }:

{
  imports = [ ./user-common.nix ];
  programs.git = {
    enable = true;
    userEmail = "me@yuanwang.ca";
    extraConfig = {
      github.user = "yuanw";
    };
  };
}
