{ ... }:
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

      "http://jelly.home" = {
        serverAliases = [ "http://www.jelly.home" ];
        extraConfig = ''
          reverse_proxy localhost:8096
          # tls internal
        '';
      };
    };

  };
}
