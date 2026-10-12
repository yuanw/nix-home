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
  # eth1 to the throughput client (a Mac, 10.1.0.2). Declared rather than left to
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
    ];
  };

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
}
