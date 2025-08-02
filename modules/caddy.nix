{ ... }:
let
  certloc = "/var/lib/acme/minilla.store";
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

      "minilla.store" = {
        extraConfig = ''
          reverse_proxy localhost:8096
          tls ${certloc}/cert.pem ${certloc}/key.pem {
             protocols tls1.3
          }
        '';
      };
    };
  };
}
