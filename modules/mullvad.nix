{ config
, lib
, pkgs
, ...
}:

let
  cfg = config.my.mullvad;
in
{
  options.my.mullvad = {
    enable = lib.mkEnableOption "Mullvad VPN";
  };

  config = lib.mkIf cfg.enable {
    services.mullvad-vpn = {
      enable = true;
      package = pkgs.mullvad-vpn;
    };

    networking.wireguard.enable = true;

    networking.firewall = {
      checkReversePath = "loose";
      allowedUDPPorts = [ 51820 ];
    };

    environment.systemPackages = [ pkgs.mullvad-vpn ];
  };
}
