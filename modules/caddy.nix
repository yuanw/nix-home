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
      "ha.minilla.store" = {
        extraConfig = ''
          reverse_proxy localhost:8123
          tls ${certloc}/cert.pem ${certloc}/key.pem {
             protocols tls1.3
           }
        '';
      };

      "jelly.minilla.store" = {
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
