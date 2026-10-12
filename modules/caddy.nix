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
      "ha.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy localhost:8123
        '';
      };

      "jellyfin.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy localhost:8096
        '';
      };

      # LAN-only names.  dnsmasq sends *every* *.yuanw.me to misfit
      # (openwrt/routers/e8450.nix), so these just need a vhost here; no DNS
      # change.  Nothing forwards WAN 443 -> misfit, so they stay LAN-only.
      # If that ever changes, gate these: a router admin UI, a Cockpit root
      # shell, and an unauthenticated inference API must not be internet-facing.
      "openwrt.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy http://192.168.1.1
        '';
      };

      "cockpit.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy http://dgx-spark.lan:9090
        '';
      };

      # TensorFold Zig (OpenAI-compatible API, no auth) on dgx-spark:8888.
      # Same LAN-only caveat as the two above: an unauthenticated inference
      # endpoint must not become internet-reachable.
      "tensorfold.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy http://dgx-spark.lan:8888
        '';
      };
    };
  };
}
