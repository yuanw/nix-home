{ ... }:
{

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.caddy = {
    enable = true;
    globalConfig = ''
      auto_https off
    '';
    virtualHosts = {
      "http://*.yuanw.me" = {
        extraConfig = ''
          redir https://{host}{uri}
        '';
      };
      "http://ha.home" = {
        serverAliases = [ "http://www.ha.home" ];
        extraConfig = ''
          reverse_proxy localhost:8123
          # tls internal
        '';
      };

      "jellyfin.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy localhost:8096
        '';
      };
    };
  };
}
