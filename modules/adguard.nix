{ modulesPath, pkgs, config, ... }:

{

  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];

  documentation.enable = false;
  environment.systemPackages = [
    pkgs.bind
    # pkgs.lego
    pkgs.wireguard-tools
  ];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 10d";
  };

  networking.firewall = {
    # 53 for dns
    # 443 for https
    # 853 for DNS-over-TLS, DNS-over-QUIC port
    allowedTCPPorts = [ 53 443  ];
    # 51820 for wireguard
    allowedUDPPorts = [ 53 51820 ];
    # allowedUDPPortRanges = [
    #   {
    #     from = 53;
    #     to = 53;
    #   }
    #   # {
    #   #   from = 853;
    #   #   to = 853;
    #   # }
    # ];
  };
 # enable NAT
  networking.nat.enable = true;
  networking.nat.externalInterface = "eth0";
  networking.nat.internalInterfaces = [ "wg0" ];
  services.fail2ban.enable = true;
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
  networking.wireguard.interfaces = {
    # "wg0" is the network interface name. You can name the interface arbitrarily.
    wg0 = {
      # Determines the IP address and subnet of the server's end of the tunnel interface.
      ips = [ "10.100.0.1/24" ];

      # The port that WireGuard listens to. Must be accessible by the client.
      listenPort = 51820;

      # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
      # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
      '';

      # This undoes the above command
      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
      '';

      # Path to the private key file.
      #
      # Note: The private key can also be included inline via the privateKey option,
      # but this makes the private key world-readable; thus, using privateKeyFile is
      # recommended.
      privateKeyFile = config.age.secrets.wireguard-server-private-key.path;

      peers = [
        # List of allowed peers.
        { # iphone
          # Public key of the peer (not a file path).
          publicKey = "HVyTUtl0/JpL7jewFimxhb97Aku8uWLBblX9B2/VChs=";
          # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
          allowedIPs = [ "10.100.0.2/32" ];
        }
        # { # John Doe
        #   publicKey = "{john doe's public key}";
        #   allowedIPs = [ "10.100.0.3/32" ];
        # }
      ];
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
      wireguard-server-private-key = {
        file = ../secrets/wireguard-server-private.age;
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
      # filters = [{
      #   enabled = true;
      #   id = 1;
      #   url =
      #     "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
      # }];
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
