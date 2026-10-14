{
  # Our layer on top of we-are-mono/nix's gateway-native config.
  #
  # This file is the board's *router* config: WAN on eth2, the LAN bridge on
  # eth0+eth1, NAT, dnsmasq 1:1 with what the e8450 serves today. It is the
  # cutover design of docs/mono-gateway-cutover.org turned into =networking.*= and
  # =services.*=, and it replaces the lab scaffolding the earlier phases used.
  #
  # Not deployed. Until the WAN drop is physically moved, eth2 has no carrier, and
  # activating this also moves the board's address off 10.0.0.2 onto the LAN, so
  # =just mono-gateway-redeploy= is the act of cutting over, not a way to look at
  # the config. Stage it deliberately: serial or UART in hand, one interface at a
  # time. The staging address below is what keeps the old management path alive.
  #
  # IPv6 is deliberately absent (cutover decision 5: IPv4 first, PD second).
  # Today's e8450 does hand out RAs and a delegated /64, so this is a known
  # regression for one cutover window, not an oversight.

  # --- Phase 5.6 fix: stop cmm crash-looping on the ctnetlink ABI ---
  # cmm was failing on every boot with:
  #   ctnetlink kernel ABI is broken, contact your vendor.
  #   parse_mnl.c:884 reason: Numerical result out of range
  # Root cause: mono-nix's libnetfilter_conntrack patch validates the nested
  # CTA_COMCERTO_FP_XFRM_HANDLE attribute as 16 bytes (its struct is uint32_t[4]),
  # but the kernel in the same flake emits it as 8 bytes -- u16 xfrm_handle[2*2]
  # (MAX_SUPPORTED_XFRMS_PER_DIR = 2). mnl_attr_validate2() therefore returns
  # ERANGE and abi_breakage() exits, so the ASK fast path never gets programmed
  # and every flow silently falls back to software forwarding. cmm itself reads
  # the handle as uint16_t*, so the library is the odd one out: fix the struct to
  # u16[4] and the validator to 8. grep guards make a silent no-op a build error.
  nixpkgs.overlays = [
    (_final: prev: {
      mono-gateway-libnetfilter_conntrack = prev.mono-gateway-libnetfilter_conntrack.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          sed -i 's/uint32_t[[:space:]]*xfrm_handle\[4\]/uint16_t xfrm_handle[4]/' \
            include/internal/object.h
          sed -i '/nfct_parse_comcerto_fp_attr_cb/,/^}/ s/MNL_TYPE_UNSPEC, 16/MNL_TYPE_UNSPEC, 8/' \
            src/conntrack/parse_mnl.c
          grep -q 'uint16_t xfrm_handle\[4\]' include/internal/object.h
          grep -q 'mnl_attr_validate2(attr, MNL_TYPE_UNSPEC, 8)' src/conntrack/parse_mnl.c
        '';
      });
    })
    # --- Phase 5.8 fix: stop the kernel handing IPsec SAs to cmm ---
    # With cmm running, the policy-based IPsec tunnel dies: the child SAs sit at 0
    # bytes and no ESP is sent. Cause is not conntrack -- it is the NXP IPsec fast
    # path. net/xfrm/xfrm_policy.c (guarded by IPSEC_FLOW_CACHE, defined in net/xfrm/
    # via ipsec_flow.h, plus CONFIG_INET_IPSEC_OFFLOAD) pushes every SA's handles and
    # tunnel header to userspace over a NETLINK_KEY socket; keytrack.c picks that up
    # and cmmSASetTunnel -> __cmmSATunnelRegister programs FPP_CMD_IPSEC_SA_TNL_ROUTE
    # into the FMan. That assumes a *forwarded* SA, so for a locally-terminated tunnel
    # __cmmRouteRegister fails ("route sa entry add failed") yet the FPP command is
    # still issued, and the datapath is broken. cmm's ESP_PASSTHRU/LOCAL_PROTO path
    # never runs because the offload is key-engine driven, not conntrack driven.
    #
    # cmm has no runtime switch, so cut it at the source. There are two paths that
    # tell cmm about SAs: the flow-cache hook in xfrm_policy.c (two ipsec_flow_add
    # calls) and the state notify in af_key.c (pfkey_send_notify -> ipsec_nlkey_send).
    # Both must go -- patching only the first still left two handles in cmm's table.
    # Disabling CONFIG_INET_IPSEC_OFFLOAD instead does NOT build (NXP reads
    # skb->ipsec_offload unguarded from a dev_queue_xmit helper), so short-circuit
    # instead. With both cut the kernel neither fills its IPsec flow cache nor
    # notifies cmm, so the SA stays software and cmm's FLOW_NO_*_SA path skips the
    # ESP flow. Forwarding/NAT offload is CPE_FAST_PATH/CDX, a different path.
    # grep guards make a silent no-op a build failure.
    (_final: prev: {
      mono-gateway-kernel = prev.mono-gateway-kernel.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          sed -i 's/if (ipsec_flow_add(/if (0 \&\& ipsec_flow_add(/g' net/xfrm/xfrm_policy.c
          sed -i 's/ipsec_nlkey_send(net, x, c);/if (0) ipsec_nlkey_send(net, x, c);/' net/key/af_key.c
          test "$(grep -c 'if (0 && ipsec_flow_add(' net/xfrm/xfrm_policy.c)" -ge 2 || {
            echo "FAILED: xfrm_policy.c ipsec_flow_add call sites not short-circuited"
            exit 1
          }
          grep -q 'if (0) ipsec_nlkey_send(net, x, c);' net/key/af_key.c || {
            echo "FAILED: af_key.c ipsec_nlkey_send not short-circuited"
            exit 1
          }
        '';
      });
    })
  ];

  # Nothing here asks for DHCP, so declare it per interface. Without this line
  # NixOS enables DHCP on every interface that is unconfigured, which on this box
  # means the SFP+ ports and the lab links.
  networking.useDHCP = false;

  # --- WAN (cutover decision 2) ---
  # eth2. The board's physical port order is not the kernel's, and Phase 3 found
  # eth0 is the third RJ-45 from the left, so confirm this map by cable before the
  # swap. Plain DHCPv4 exactly like the e8450's =proto dhcp= -- there is no PPPoE
  # anywhere in this path. Until the ISP drop is moved to this port it has no
  # carrier and contributes nothing, which is why this is safe to deploy today.
  networking.interfaces.eth2.useDHCP = true;

  # --- LAN (cutover decisions 1 and 2) ---
  # br-lan over eth1, same subnet as the house so nothing renumbers. eth0 is NOT a
  # member: it is still the point-to-point link to this Mac at 10.0.0.2, and that
  # is how we reach the board while it is staged. Step 2 of the cutover drops eth0
  # into this bridge and deletes the static address below -- that is the act that
  # makes 192.168.1.1 the board's only address.
  networking.bridges.br-lan.interfaces = [ "eth1" ];
  networking.interfaces.br-lan.ipv4.addresses = [
    {
      address = "192.168.1.1";
      prefixLength = 24;
    }
  ];

  # Management link, unchanged from the lab config so the deploy path survives
  # being staged. No defaultGateway and no nameservers on it: the board is
  # air-gapped on purpose (decided 2026-10-11) and closures are pushed from
  # dgx-spark, so nothing here should ever pull.
  networking.interfaces.eth0 = {
    ipv4.addresses = [
      {
        address = "10.0.0.2";
        prefixLength = 24;
      }
    ];
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY"
  ];

  # --- NAT ---
  # What the e8450 does with `masq` and `mtu_fix` on its wan zone. The NixOS nat
  # module also emits the forwarding rule lan -> wan, which is the equivalent of
  # OpenWrt's firewall.forwarding entry.
  networking.nat = {
    enable = true;
    externalInterface = "eth2";
    internalInterfaces = [ "br-lan" ];
  };

  # --- DNS + DHCP (cutover decision 4: dnsmasq 1:1, kea/resolved later) ---
  # Only on the LAN: without `interface` dnsmasq binds 0.0.0.0 and would answer
  # on the WAN side too. Upstream servers are not listed here on purpose -- the
  # NixOS module feeds dnsmasq whatever the WAN's DHCP hands out, which is what
  # the e8450's dnsmasq forwards to.
  services.dnsmasq = {
    enable = true;
    # false on purpose. With the default (true) NixOS rewrites the router's own
    # resolv.conf to 127.0.0.1 and feeds dnsmasq through resolvconf; here the WAN
    # already puts the ISP resolvers in /etc/resolv.conf via dhcpcd, which is
    # exactly what the e8450's dnsmasq forwards to.
    resolveLocalQueries = false;
    settings = {
      interface = "br-lan";
      bind-interfaces = true;
      domain-needed = true;
      bogus-priv = true;

      # start 100 + limit 150 + 12h, the same window as the e8450's dhcp.lan
      # (.100 through .249), so a lease that survives the swap comes back from the
      # same pool.
      dhcp-range = "192.168.1.100,192.168.1.249,12h";

      # Static leases, read 1:1 out of openwrt/routers/e8450.nix. dgx-spark has no
      # IPv4 reservation there (only a DUID and hostid, which is IPv6), so it is
      # intentionally not in this list; it keeps landing on .185 via DHCP.
      dhcp-host = [
        "DA:E4:6A:44:49:09,192.168.1.212,Mac"
        "58:47:CA:7E:C0:05,192.168.1.190,misfit"
      ];

      # option 3 = gateway, 6 = dnsmasq itself on .1, the same two the e8450 hands out.
      dhcp-option = [
        "3,192.168.1.1"
        "6,192.168.1.1"
      ];
    };
  };

  # --- Firewall ---
  # Only the two rules that change behaviour, both copied from fw4's wan zone.
  # Everything else is already what NixOS does: input denied from WAN, masquerade,
  # forwarding accepted between the bridge and the WAN. Ping stays allowed from
  # everywhere, where fw4 only allowed it from wan.
  networking.firewall = {
    # Allow-DHCP-Renew. Without this the lease renews on .100 but never commits.
    allowedUDPPorts = [ 68 ];
  };
}
