{ pkgs, ... }:

{
  imports = [ ./user-common.nix ];
  programs.git = {
    enable = true;
    userEmail = "me@yuanwang.ca";

    signing = {
      key = "9254E38FE868F77C";
      signByDefault = true;
    };

    extraConfig = {
      github.user = "yuanw";
    };
  };
}
