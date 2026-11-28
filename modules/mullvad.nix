{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.my.mullvad;

  ns = "seeder";
  vethHost = "sd-h";
  vethNs = "sd-s";
  netA = "10.240.0.1";
  netB = "10.240.0.2";
  netCidr = "10.240.0.0/24";
  mullvadDns = "10.64.0.1";
in
{
  options.my.mullvad = {
    enable = lib.mkEnableOption ''
      a Mullvad WireGuard exit that only the seeder can see — qBittorrent runs in
      its own network namespace with the tunnel inside it, so "no tunnel" means "no
      route", which is the only kill switch that has ever actually worked
    '';

    privateKeyFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to the WireGuard private key (an agenix path).";
    };

    address = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = ''
        Tunnel addresses Mullvad assigned to this device, CIDR form, from
        `POST https://api.mullvad.net/wg` with `account` + `pubkey`. Not chosen
        locally: re-registering the device means a new value here.
      '';
    };

    relayHost = lib.mkOption {
      type = lib.types.str;
      description = "IPv4 of the Mullvad relay. Picked by RTT, not by default.";
    };

    relayPort = lib.mkOption {
      type = lib.types.port;
      default = 51820;
    };

    relayPublicKey = lib.mkOption {
      type = lib.types.str;
      description = "The relay's WireGuard public key, from api.mullvad.net.";
    };
  };

  config = lib.mkIf cfg.enable {
    boot.kernelModules = [ "wireguard" ];
    environment.systemPackages = [ pkgs.wireguard-tools ];

    # The namespace's only path to the relay is through here: the WireGuard
    # *outer* packets are forwarded by the host, so forwarding has to be on.
    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

    # One oneshot owns every piece of namespace state, and tears all of it down on
    # stop, so `systemctl stop` returns the box to the pre-Phase-5 shape with no
    # leftover links, routes or NAT rules.
    systemd.services.seeder-netns = {
      before = [ "qbittorrent.service" ];
      wantedBy = [ "multi-user.target" ];
      # NetworkManager churns links; make sure it cannot win a race against us.
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };

      script = ''
        set -e
        ip=${pkgs.iproute2}/bin/ip
        wg=${pkgs.wireguard-tools}/bin/wg
        nft=${pkgs.nftables}/bin/nft

        # --- namespace -------------------------------------------------------
        $ip netns add ${ns} 2>/dev/null || true
        $ip link del ${vethHost} 2>/dev/null || true
        $ip link add ${vethHost} type veth peer name ${vethNs}
        $ip link set ${vethNs} netns ${ns}

        # --- inside the namespace -------------------------------------------
        # Everything below runs where qBittorrent will run. Note there is NO
        # default route via the veth: the only default is the tunnel, so a dead
        # tunnel is a black hole rather than a leak. That structural absence is
        # the kill switch — no firewall rules carrying that responsibility.
        $ip netns exec ${ns} ${pkgs.bash}/bin/bash -c '
          set -e
          ip=${pkgs.iproute2}/bin/ip
          wg=${pkgs.wireguard-tools}/bin/wg
          nft=${pkgs.nftables}/bin/nft

          $ip link set lo up
          $ip link set ${vethNs} up
          $ip addr add ${netB}/24 dev ${vethNs}

          $ip link add dev mullvad type wireguard
          $ip addr add ${builtins.head cfg.address} dev mullvad
          ${lib.optionalString (
            builtins.length cfg.address > 1
          ) "$ip addr add ${builtins.elemAt cfg.address 1} dev mullvad"}
          $ip link set mullvad up

          $wg set mullvad private-key ${cfg.privateKeyFile}
          $wg set mullvad peer ${cfg.relayPublicKey} \
            endpoint ${cfg.relayHost}:${toString cfg.relayPort} \
            allowed-ips 0.0.0.0/0 \
            persistent-keepalive 25

          $ip route replace default dev mullvad
          # …and the one exception: the relay itself must reach the host, or the
          # tunnel can never carry its own handshake.
          $ip route replace ${cfg.relayHost}/32 via ${netA} dev ${vethNs}

          # Tracker and peer DNS belongs to Mullvad inside the tunnel; without
          # this the payload is encrypted while the ISP still learns every
          # hostname the seeder resolves.
          $nft add table ip seeder_dns 2>/dev/null || true
          $nft add chain ip seeder_dns out "{ type nat hook output priority -100; policy accept; }"
          $nft add rule ip seeder_dns out udp dport 53 dnat ip to ${mullvadDns}:53
          $nft add rule ip seeder_dns out tcp dport 53 dnat ip to ${mullvadDns}:53
        '

        # --- host side -------------------------------------------------------
        $ip addr add ${netA}/24 dev ${vethHost}
        $ip link set ${vethHost} up
        $ip -6 addr add fe80::1/64 dev ${vethHost} 2>/dev/null || true

        # Only the WireGuard outer packets ever reach the host, so this MASQUERADE
        # covers exactly one flow: the tunnel's own UDP to the relay. The inner
        # torrent packets are consumed by the tunnel device inside the namespace.
        $nft add table ip seeder_nat 2>/dev/null || true
        $nft add chain ip seeder_nat postrouting "{ type nat hook postrouting priority 100; policy accept; }" 2>/dev/null || true
        $nft add rule ip seeder_nat postrouting ip saddr ${netCidr} oifname != "${vethHost}" masquerade

        # NixOS forwards nothing by default; let the namespace's traffic through.
        $nft add table inet seeder_fwd 2>/dev/null || true
        $nft add chain inet seeder_fwd forward "{ type filter hook forward priority 0; policy drop; }" 2>/dev/null || true
        $nft add rule inet seeder_fwd forward iifname "${vethHost}" ip saddr ${netCidr} accept
        $nft add rule inet seeder_fwd forward oifname "${vethHost}" ct state established,related accept
      '';

      serviceConfig.ExecStop = pkgs.writeShellScript "seeder-netns-down" ''
        ${pkgs.iproute2}/bin/ip netns del ${ns} 2>/dev/null || true
        ${pkgs.iproute2}/bin/ip link del ${vethHost} 2>/dev/null || true
        ${pkgs.nftables}/bin/nft delete table ip seeder_nat 2>/dev/null || true
        ${pkgs.nftables}/bin/nft delete table inet seeder_fwd 2>/dev/null || true
        # The DNS table lives in the namespace, so deleting the namespace takes it.
      '';
    };

    # The seeder lives in the namespace. This is what makes the whole thing work:
    # it is the same trick as `network_mode: service:gluetun` in the docker-compose
    # stacks, and it exists because nftables cannot re-route an already-routed
    # locally-generated packet — `meta mark set` in the output hook stamps the
    # packet but never changes where it goes. Measured on this box: 291 packets
    # marked, zero of them entering the tunnel.
    systemd.services.qbittorrent = {
      after = [ "seeder-netns.service" ];
      requires = [ "seeder-netns.service" ];
      # [Service], not [Unit] — it is a systemd.exec(5) setting. In [Unit] systemd
      # logs "Unknown key 'NetworkNamespacePath' in section [Unit], ignoring" and
      # runs the service in the host namespace, which looks exactly like a working
      # tunnel while the seeder leaks. Verified with `systemctl show -p
      # NetworkNamespacePath` and by comparing /proc/<pid>/ns/net against the inode
      # of /run/netns/seeder.
      serviceConfig.NetworkNamespacePath = "/run/netns/${ns}";
      # The unit gets the namespace's loopback as "localhost", so the WebUI is
      # reached from the host at ${netB}:8080 — which is also what Prowlarr's
      # download-client entry has to say now.
    };
  };
}
