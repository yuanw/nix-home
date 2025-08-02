{ ... }:
let
  certloc = "/var/lib/acme/yuanw.me";
in
{

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.caddy = {
    enable = true;
    virtualHosts = {
      "http://ha.home" = {
        serverAliases = [ "http://www.ha.home" ];
        extraConfig = ''
          reverse_proxy localhost:8123
          # tls internal
        '';
      };

      "jellyfin.yuanw.me" = {
        extraConfig = ''
          reverse_proxy :8096
          tls ${certloc}/cert.pem ${certloc}/key.pem
        '';
      };
    };
  };
}
