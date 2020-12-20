{ lib, ... }: {
  imports = [ ../../modules/macintosh.nix ];
  networking.hostName = "yuan-mac";
  programs.stevenBlackHosts.enable = true;
  programs.editors.emacs = {
    enable = true;
    enableDoomConfig = false;
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
