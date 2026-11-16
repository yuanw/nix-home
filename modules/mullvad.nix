{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.my.mullvad;
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    types
    md
    ;

  # The mark that pulls the seeder into the tunnel. Distinct from anything the
  # WireGuard backend uses, so the tunnel's own handshake keeps using the main
  # table and can never recurse into itself.
  mark = "0xaa";
  table = "mullvad";

  seederUid = config.users.users.${cfg.user}.uid;

  rules = pkgs.writeText "mullvad-split.nft" ''
    table inet mullvad_split {
      # Locally generated packets from the seeder's uid get the mark; the policy
      # rule below then sends marked traffic to table "${table}", which is where the
      # WireGuard interface puts its default route. Marking is by uid, so this can
      # never touch Jellyfin, hass, caddy or your own shell — different uid, the
      # rule is not even evaluated for them.
      chain mark {
        type filter hook output priority -150; policy accept;
        meta skuid ${toString seederUid} meta mark set ${mark}
      }

      # Kill switch. NOT Mullvad's documented one: their wg-quick recipe REJECTs
      # every OUTPUT packet not leaving via the tunnel for *every* uid, which on
      # this box would take down Jellyfin, hass and your SSH session the moment the
      # tunnel wobbled. Scoped to the seeder's uid instead.
      chain kill {
        type filter hook output priority 0; policy accept;
        # Everyone else is none of our business.
        meta skuid != ${toString seederUid} accept
        # Already inside the tunnel.
        oifname "mullvad" accept
        # Replies. Without this the WebUI on :8080 dies too — its answers are
        # locally-generated packets from this same uid, so a naive drop locks you
        # out of the client you are trying to protect.
        ct state established,related accept
        # The tunnel's own handshake, so a dead tunnel can never stay dead.
        ip daddr ${cfg.relayHost} udp dport ${toString cfg.relayPort} accept
        ip6 daddr ${cfg.relayHost} udp dport ${toString cfg.relayPort} accept
        oifname "lo" accept
        # Drop the rest: no egress for the seeder outside the tunnel, DNS included.
        meta skuid ${toString seederUid} drop
      }

      # DNS. The seeder resolves tracker hostnames through the system resolver,
      # which on this box means the router's AdGuard Home and then the ISP — i.e.
      # the payload is encrypted and your ISP still learns every tracker you dial.
      # Redirect that uid's DNS into the tunnel to Mullvad's resolver instead,
      # without touching anyone else's resolution (hass LAN discovery depends on it).
      chain dns {
        type nat hook output priority -100; policy accept;
        meta skuid ${toString seederUid} udp dport 53 redirect to 10.64.0.1
        meta skuid ${toString seederUid} tcp dport 53 redirect to 10.64.0.1
      }
    }
  '';
in
{
  options.my.mullvad = {
    enable = mkEnableOption "a uid-scoped Mullvad WireGuard exit for the seeder";

    user = mkOption {
      type = types.str;
      default = "qbittorrent";
      description = md ''
        The only account whose traffic is tunnelled. Its uid must be **static**:
        a `DynamicUser` gets a fresh uid every start, the `meta skuid` matches
        would stop matching, and the kill switch would silently stop protecting
        while everything still looked healthy.
      '';
    };

    privateKeyFile = mkOption {
      type = types.str;
      description = "Path to the WireGuard private key (an agenix path).";
    };

    address = mkOption {
      type = types.listOf types.str;
      description = ''
        Tunnel addresses Mullvad assigned to this device, in CIDR form. Returned by
        `POST https://api.mullvad.net/wg` with `account` + `pubkey`; not chosen
        locally, so a re-registered device means a new value here.
      '';
    };

    relayHost = mkOption {
      type = types.str;
      description = "IPv4 of the Mullvad relay. Picked by RTT, not by default.";
    };

    relayPort = mkOption {
      type = types.port;
      default = 51820;
    };

    relayPublicKey = mkOption {
      type = types.str;
      description = "The relay's WireGuard public key, from api.mullvad.net.";
    };

    killSwitch = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Drop the seeder's traffic when the tunnel is down. Off means a dropped
        tunnel silently reseeds from the home IP, which is the failure this whole
        phase exists to prevent.
      '';
    };
  };

  config = mkIf cfg.enable {
    networking.wireguard.interfaces.mullvad = {
      ips = cfg.address;
      privateKeyFile = cfg.privateKeyFile;

      # The default here is "main", which would install a default route for the
      # whole box and take Jellyfin, hass and your SSH session through Norway.
      # A named table means only fwmarked traffic ever reaches these routes.
      table = table;

      peers = [
        {
          publicKey = cfg.relayPublicKey;
          endpoint = "${cfg.relayHost}:${toString cfg.relayPort}";
          # Broad, because nothing is routed here except marked traffic.
          allowedIPs = [
            "0.0.0.0/0"
            "::/0"
          ];
          # The seeder idles between handshakes; keep the NAT mapping alive.
          persistentKeepalive = 25;
        }
      ];
    };

    systemd.services.mullvad-split-tunnel = {
      after = [ "wireguard-mullvad.service" ];
      requires = [ "wireguard-mullvad.service" ];
      wantedBy = [ "multi-user.target" ];
      # Rules and routes are kernel state, not unit state: re-apply on every start
      # so a reboot or a re-deploy cannot leave the box half-wired.
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        # Absolute paths rather than a PATH gamble: this unit has to work when it is
        # the thing the box depends on, and a oneshot with no PATH in scope failing
        # at boot is exactly the silent-leak scenario the kill switch exists for.
        ExecStart = pkgs.writeShellScript "mullvad-split-up" ''
          ${pkgs.iproute2}/bin/ip rule del fwmark ${mark} table ${table} 2>/dev/null || true
          ${pkgs.iproute2}/bin/ip rule add fwmark ${mark} table ${table} priority 100
          ${pkgs.nftables}/bin/nft -f ${rules}
        '';
        ExecStop = pkgs.writeShellScript "mullvad-split-down" ''
          ${pkgs.nftables}/bin/nft delete table inet mullvad_split 2>/dev/null || true
          ${pkgs.iproute2}/bin/ip rule del fwmark ${mark} table ${table} 2>/dev/null || true
        '';
      };
    };

    # The seeder should come up after the wiring, not before it, so its first
    # tracker lookups are already inside the tunnel.
    systemd.services.qbittorrent.after = [ "mullvad-split-tunnel.service" ];
    systemd.services.qbittorrent.requires = [ "mullvad-split-tunnel.service" ];
  };
}
