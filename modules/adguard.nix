{ modulesPath, pkgs, config, ... }:

{
  environment.systemPackages = [
    pkgs.dig
    # pkgs.lego
    pkgs.wireguard-tools
  ];

  networking.firewall = {
    # 53 for dns
    # 443 for https
    # 853 for DNS-over-TLS, DNS-over-QUIC port
    allowedTCPPorts = [ 53 443 ];
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
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/services/networking/nat.nix#L35
  networking.nat = {
    enable = true;
    # enableIPv6 = true;
    # https://www.baeldung.com/linux/network-interface-configure
    externalInterface = "ens5";
    internalInterfaces = [ "wg0" ];
    # internalIPs = [ "10.42.0.0/16" ];
    # internalIPv6s = [ "fd42::/16" ];
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
  networking.wg-quick.interfaces = {
    # "wg0" is the network interface name. You can name the interface arbitrarily.
    wg0 = {
      # Determines the IP/IPv6 address and subnet of the client's end of the tunnel interface
      # address = [ "10.0.0.1/24" "fdc9:281f:04d7:9ee9::1/64" ];
      address = [ "10.100.0.1/24"
                ];
      # The port that WireGuard listens to - recommended that this be changed from default
      listenPort = 51820;
      # Path to the server's private key
      privateKeyFile = config.age.secrets.wireguard-server-private-key.path;
      # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
      postUp = ''
         ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o ens5 -j MASQUERADE
      '';

      # Undo the above
      preDown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o ens5 -j MASQUERADE
      '';

      peers = [
        { # peer0
          publicKey = "HVyTUtl0/JpL7jewFimxhb97Aku8uWLBblX9B2/VChs=";
          allowedIPs = [ "10.100.0.2/32" ];
        }
        # More peers can be added here.
      ];
    };
  };
  # boot.kernel.sysctl."net.ipv4.ip_forward" = "1";
  # boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = "1";
  # networking.dhcpcd = {
  #   wait = "ipv4";
  #   extraConfig = "noipv4ll";
  # };
  # networking.dhcpcd.enable = false;
  # networking.useDHCP = false;
  # networking.wireguard.interfaces = {
  #   # "wg0" is the network interface name. You can name the interface arbitrarily.
  #   wg0 = {
  #     # Determines the IP address and subnet of the server's end of the tunnel interface.
  #     ips = [ "10.100.0.1/24" ];

  #     # The port that WireGuard listens to. Must be accessible by the client.
  #     listenPort = 51820;

  #     # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
  #     # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
  #     postSetup = ''
  #       ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
  #     '';

  #     # This undoes the above command
  #     postShutdown = ''
  #       ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
  #     '';

  #     # Path to the private key file.
  #     #
  #     # Note: The private key can also be included inline via the privateKey option,
  #     # but this makes the private key world-readable; thus, using privateKeyFile is
  #     # recommended.
  #     privateKeyFile = config.age.secrets.wireguard-server-private-key.path;

  #     peers = [
  #       # List of allowed peers.
  #       {
  #         # iphone
  #         # Public key of the peer (not a file path).
  #         publicKey = "HVyTUtl0/JpL7jewFimxhb97Aku8uWLBblX9B2/VChs=";
  #         # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
  #         allowedIPs = [ "10.100.0.2/32" ];
  #         # https://github.com/NixOS/nixpkgs/issues/63869
  #         dynamicEndpointRefreshSeconds = 5;
  #       }
  #       # { # John Doe
  #       #   publicKey = "{john doe's public key}";
  #       #   allowedIPs = [ "10.100.0.3/32" ];
  #       # }
  #     ];
  #   };
  # };


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
