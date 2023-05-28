{ modulesPath, pkgs, config, ... }:

{

  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];

  documentation.enable = false;
  environment.systemPackages = [ pkgs.bind pkgs.lego ];
  networking.firewall = {
    # 53 for dns
    # 443 for https
    # 853 for DNS-over-TLS, DNS-over-QUIC port
    allowedTCPPorts = [ 53 443 853 ];
    allowedUDPPortRanges = [
      {
        from = 53;
        to = 53;
      }
      {
        from = 853;
        to = 853;
      }
    ];
  };
  # https://nlnetlabs.nl/documentation/unbound/unbound.conf/
  services.unbound = {
    enable = true;
    settings = {
      server = {
        hide-identity = "yes";
        hide-version = "yes";
        port = 5335;
      };
    };
  };

  age = {
    secrets = {
      adguard = {
        file = ../secrets/adguard.age;
        mode = "770";
        owner = "adguardhome";
      };
      adguard-encryption-private-key = {
        file = ../secrets/adguard-encryption-key.age;
        mode = "770";
        path = "/opt/adguradhome/my.key";
        owner = "adguardhome";
      };
   adguard-encryption-certificate = {
        file = ../secrets/adguard-encryption-certificate.age;
        mode = "770";
        path = "/opt/adguradhome/my.crt";
        owner = "adguardhome";
      };

    };
  };

  modules = { secrets.agenix = { enable = true; }; };
  services.adguardhome-with-user = {
    # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/services/networking/adguardhome.nix#L135
    enable = true;
    openFirewall = true;
    mutableSettings = true;
    user = "test";
    passwordFile = config.age.secrets.adguard.path;
    # corresponds to /var/lib/AdGuardHome/AdGuardHome.yaml
    settings = {
      block_auth_min = 10;
      # debug_pprof = true;
      filters = [{
        enabled = true;
        id = 1;
        url =
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
      }];
      dns = {
        bind_hosts = [ "0.0.0.0" ];

        # query logging
        querylog_enabled = true;
        querylog_file_enabled = true;
        querylog_interval = "24h";
        querylog_size_memory = 1000; # entries
        anonymize_client_ip = false; # for now

        # adguard
        protection_enabled = true;
        blocking_mode = "default"; # NXDOMAIN
        filtering_enabled = true;
        blocked_services =
          [ "bilibili" "ok" "snapchat" "tiktok" "tinder" "9gag" "twitch" ];
        # upstream DNS
        upstream_dns = [ "127.0.0.1:5335" ];
        # if upstream has any hostnames
        bootstrap_dns = [ "192.168.1.1" ]; # ask the gateway
        # caching
        cache_size = 536870912; # 512 MB
        cache_ttl_min = 1800; # 30 min
        cache_optimistic = true; # return stale and then refresh
      };
    };
  };

  system.stateVersion = "22.11";

}
