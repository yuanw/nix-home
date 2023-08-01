{ modulesPath, ... }:

{

  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  documentation.enable = false;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 10d";
  };

  services.fail2ban.enable = true;
}
