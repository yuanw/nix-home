{
  # Our layer on top of we-are-mono/nix's gateway-native config.
  #
  # Upstream's configurations/gateway.nix imports hardware/networking/ask-offload
  # and deliberately leaves interfaces unconfigured -- with no interface declared,
  # NixOS enables DHCP on all of them. That is fine on a real LAN and useless on
  # the direct link this board is currently cabled to, where it falls back to
  # 169.254/16 and the board is unreachable after a reboot.
  #
  # This file is also the home for the root SSH key: upstream keeps the list empty
  # and comments that it is "managed manually on the device". Hand-editing
  # /root/.ssh/authorized_keys does not survive a rebuild, so put the key here.

  # --- LAN ---
  # Point-to-point with the development Mac (10.0.0.1), no gateway on this link.
  # eth0 is the third RJ-45 from the left -- see Phase 3 of the build plan.
  networking.useDHCP = false;
  networking.interfaces.eth0 = {
    ipv4.addresses = [
      {
        address = "10.0.0.2";
        prefixLength = 24;
      }
    ];
  };

  # --- Phase 5.5 test link ---
  # eth1 to the throughput client (asche, 10.1.0.24). Declared rather than left to
  # `ip link set up`, because an unconfigured interface is never brought up, and
  # carrier reports from the FMan driver cannot be trusted without it. Nothing here
  # is production addressing -- this subnet exists for iperf3 and should be replaced
  # when the cutover assigns real WAN/LAN roles.
  networking.interfaces.eth1 = {
    ipv4.addresses = [
      {
        address = "10.1.0.1";
        prefixLength = 24;
      }
      # Inner address of the 5.8 tunnel. /32 on the lab interface, not lo: strongSwan
      # sources the inner packet from this address and only then does the outbound
      # policy match; charon installs the route to the peer /32 itself. Delete with the
      # rest of the 5.8 block.
      {
        address = "10.90.0.1";
        prefixLength = 32;
      }
    ];
  };

  # charon needs the inner address present before it installs routes/policies.
  systemd.services.strongswan-swanctl.after = [ "network-addresses-eth1.service" ];

  # --- Access ---
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY"
  ];

  # Air-gapped on purpose (decided 2026-10-11): eth0 has only the on-link route, no
  # gateway, no DNS. Closures are pushed from dgx-spark via `just
  # mono-gateway-redeploy`, which is all this board needs; nothing here ever pulls.
  # Do not "fix" this by adding defaultGateway/DNS -- the day this becomes the
  # router, WAN addressing falls out of that config properly and the stopgap NAT
  # through the Mac stops mattering. The only reason to want internet here is
  # on-device kernel iteration (build plan Option C), deferred until we hit a bug a
  # rebuild cannot reproduce.
  #
  # Separately, there is no `nix.enable` here: it already defaults to true and the
  # daemon is socket-activated, so it reads "inactive" until a client connects.

  # --- Phase 5.5 lab firewall exceptions ---
  # Two rules, both needed before any iperf traffic moves, and both invisible until
  # you go looking. The generated ruleset RP-filters via `fib saddr . iif check` in a
  # chain whose policy is drop and whose exception chain is empty, so a link-local
  # source is dropped in prerouting before any accept rule is consulted. Its input
  # chain accepts tcp 22 and the temp-port set only, so iperf's port falls through to
  # the drop policy. Both present as "the gateway is broken", never "packet dropped".
  #
  # Temporary scaffolding. Delete this whole service at cutover, when zones carry real
  # subnets and RP-filter has something real to match against.
  systemd.services.gw-lab-firewall = {
    description = "Phase 5.5 lab firewall exceptions (remove at cutover)";
    after = [
      "nftables.service"
      "network-addresses-eth1.service"
    ];

    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      # List form, so nft's arguments go straight to argv. There is no shell here and
      # therefore nothing to quote -- ruleset names contain no spaces.
      ExecStart = [
        "/run/current-system/sw/bin/nft insert rule inet nixos-fw rpfilter-allow iifname eth1 accept"
        "/run/current-system/sw/bin/nft insert rule inet nixos-fw input-allow iifname eth1 tcp dport 5201 accept"
        "/run/current-system/sw/bin/nft insert rule inet nixos-fw input-allow iifname eth0 tcp dport 5201 accept"
        # IPsec needs a new-flow accept: conntrack covers the replies to our own
        # IKE_SA_INIT, but not a fresh one arriving first (misfit reboot). ESP has no
        # port, hence the raw rule.
        "/run/current-system/sw/bin/nft insert rule inet nixos-fw input-allow iifname eth1 udp dport 500 accept"
        "/run/current-system/sw/bin/nft insert rule inet nixos-fw input-allow iifname eth1 udp dport 4500 accept"
        "/run/current-system/sw/bin/nft insert rule inet nixos-fw input-allow iifname eth1 ip protocol esp accept"
      ];
    };
  };

  # --- Phase 5.8 IPsec lab tunnel (temporary) ---
  # The other end of the 5.8 test is misfit, which carries the mirrored connection
  # definition in its own host config. Inner addressing is a pair of /32s rather than the
  # 10.1.0.0/24 the tunnel rides on: tunneling a subnet over itself is a routing loop.
  #
  # Keys are on disk under /etc/swanctl and never in this file, because the module builds
  # swanctl.conf with pkgs.writeText and anything placed in its options ends up
  # world-readable in the store. Delete this block with gw-lab-firewall once 5.8 is closed.
  services.strongswan-swanctl.includes = [ "/etc/swanctl/conf.d/lab.conf" ];

  # hw_offload is the plugin option in this build -- the child section has no xfrm_offload
  # keyword, and 6.0.4's swanctl exposes no offload keyword at all, so it belongs here and
  # nowhere else. extraConfig is the *whole* strongswan.conf: the first version of this
  # block set load_modular explicitly and broke charon-systemd with
  # "failed to load 3 critical plugin features" -- that replaced the compiled defaults
  # instead of adding to them. Leave the defaults alone and override just this.
  services.strongswan-swanctl.strongswan.extraConfig = ''
    charon {
      plugins {
        kernel-netlink {
          hw_offload = yes
        }
      }
    }
  '';

  environment.etc."swanctl/conf.d/lab.conf".text = ''
    connections {
      lab {
        version = 2
        local_addrs = 10.1.0.1
        remote_addrs = 10.1.0.2
        local {
          auth = pubkey
          id = 10.1.0.1
          certs = gateway-ca
        }
        remote {
          auth = pubkey
          id = 10.1.0.2
        }
        children {
          lab {
            local_ts = 10.90.0.1/32
            remote_ts = 10.90.0.2/32
            mode = tunnel
            start_action = start
            close_action = restart
          }
        }
      }
    }
  '';
}
