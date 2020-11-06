{ lib, ... }: {
  imports = lib.attrValues (import ../../modules);
  networking.hostName = "yuan-mac";
  programs.stevenBlackHosts.enable = true;
  programs.editors.emacs = {
    enable = true;
    enableDoomConfig = true;
  };
  home-manager.users.yuanwang.programs = {
    pet.enable = true;

    git = {
      userEmail = "me@yuanwang.ca";

      signing = {
        key = "BF2ADAA2A98F45E7";
        signByDefault = true;
      };

      extraConfig = { github.user = "yuanw"; };
    };
  };
}
