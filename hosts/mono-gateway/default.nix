{
  # Our layer on top of we-are-mono/nix's gateway-native config.
  #
  # This file is the board's *router* config: WAN on eth2, the LAN bridge on
  # eth1 (eth0 stays the management link), NAT, dnsmasq 1:1 with what the e8450
  # serves today. It is the cutover design of docs/mono-gateway-cutover.org
  # turned into =networking.*= and =services.*=, and it replaces the lab
  # scaffolding the earlier phases used.
  #
  # Staged 2026-10-14: activated on the board with eth1 unplugged.
  # eth0 keeps 10.0.0.2/24 through activation, so the management path survives;
  # eth2 has no carrier until the WAN drop is physically moved. The swap is then
  # the physical act: WAN to eth2, eth1 into the house switch, and the e8450
  # demoted to a dumb AP (docs/mono-gateway-cutover.org, *Cutover runbook*).
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
  # eth2. The board's physical port order is not the kernel's: the 2026-10-17 plug
  # check (first true gen-15 boot) found left -> right = eth0 | eth1 | eth2, so eth2
  # is the rightmost RJ-45. Identify a port by carrier, not position. Plain
  # DHCPv4 exactly like the e8450's =proto dhcp= -- there is no PPPoE
  # anywhere in this path. Until the ISP drop is moved to this port it has no
  # carrier and contributes nothing, which is why this is safe to deploy today.
  networking.interfaces.eth2.useDHCP = true;

  # --- LAN (cutover decisions 1 and 2) ---
  # br-lan over eth1, same subnet as the house so nothing renumbers. From the
  # swap on, eth0 (the Mac's mgmt port) is a member too: the fold drops it into
  # the bridge and deletes its staging address, which makes 192.168.1.1 the
  # board's only address. Two consequences: activating this from the Mac kills
  # the 10.0.0.2 ssh path mid-activation (serial in hand, runbook), and until
  # eth1 has a house uplink this config leaves br-lan with no carrier -- so it
  # is build-only until the window, never staged-activated.
  networking.bridges.br-lan.interfaces = [
    "eth1"
    "eth0"
  ];
  networking.interfaces.br-lan.ipv4.addresses = [
    {
      address = "192.168.1.1";
      prefixLength = 24;
    }
  ];

  # --- Management (staging): deleted at the swap ---
  # The 10.0.0.2/24 on eth0 was the staging path to this Mac; the fold puts eth0
  # in br-lan, so the address is gone and nothing replaces it: the board is
  # reachable at 192.168.1.1 (house LAN) or the ttyS0 console afterwards. No
  # defaultGateway/nameservers were ever set here on purpose -- air-gapped
  # (decided 2026-10-11); closures are pushed, nothing on the board pulls.

  # --- Nix garbage collection ---
  # Nothing cleaned the store: gc.automatic was never set and there is no
  # nix-gc.timer on the board, so every redeploy left a generation behind
  # (~15 MB of unique paths). Same knobs as modules/nixos_system.nix, the house
  # default. Weekly is plenty for a box that redeploys a handful of times a
  # month, and 10d keeps the current generation plus whatever was live last
  # week for rollback. 10.0.0.x/air-gapped: GC runs locally, no substituters.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 10d";
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
  # the e8450's dnsmasq forwards to. (Since AdGuard Home, the =server= catch-all
  # below wins over those, so this is only what AGH asks last.)
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
      #
      # ...which is why =dgx-spark.lan= resolved on the e8450 and not here: OpenWrt
      # appended =.lan= to every lease hostname (=local=/lan/= + =domain=lan= +
      # =add_local_hostname=) and misfit's caddy proxies two vhosts at that name.
      # A name built off a lease cannot be pinned to an address that is itself a
      # lease, so dgx-spark gets a reservation here and the two pieces come together.
      dhcp-host = [
        "DA:E4:6A:44:49:09,192.168.1.212,Mac"
        "58:47:CA:7E:C0:05,192.168.1.190,misfit"
        "4C:BB:47:29:81:69,192.168.1.185,dgx-spark"
      ];

      # the =.lan= half: dnsmasq serves =<hostname>.lan= for every name it knows.
      domain = "lan";
      expand-hosts = true;

      # option 3 = gateway, 6 = dnsmasq itself on .1, the same two the e8450 hands out.
      dhcp-option = [
        "3,192.168.1.1"
        "6,192.168.1.1"
      ];

      # The wildcard override that lived on the e8450 -- its
      # =dhcp.@dnsmasq[0].address= was =/.yuanw.me/192.168.1.190=, which the dumb-AP
      # apply switched off with the rest of that box's dnsmasq. Nothing under
      # =*.yuanw.me= resolves on the LAN without it: misfit's caddy serves the
      # LAN-only names behind that wildcard, so this is what makes
      # openwrt./cockpit./tensorfold. reach it again.
      address = "/.yuanw.me/192.168.1.190";

      # AdGuard Home as a filter hop behind dnsmasq (cutover decision 7). Only
      # queries dnsmasq does not already own an answer for reach it, so leases,
      # =.lan= names and =*.yuanw.me= are untouched and a dead AGH degrades to
      # unfiltered resolution instead of killing DNS. Port 5354, not 53, so AGH
      # can never race dnsmasq for =:53= across a restart.
      server = [ "/#/127.0.0.1#5354" ];
    };
  };

  # --- AdGuard Home (cutover decision 7) ---
  # dnsmasq stays the front door; AGH is a loopback-only filter hop behind it, so
  # there is no firewall rule to add and nothing for clients to configure. Loopback
  # binds cannot be reached from eth2, hence openFirewall = false.
  #
  # host/port are service options, NOT settings.bind_host -- current nixpkgs
  # asserts bind_host away, and host's default is 0.0.0.0, so declaring it *is*
  # the exposure decision. The UI is reached over a tunnel:
  #   ssh -N -L 3000:127.0.0.1:3000 root@192.168.1.1
  services.adguardhome = {
    enable = true;
    openFirewall = false;
    host = "127.0.0.1";
    port = 3000;
    settings = {
      dns = {
        bind_hosts = [ "127.0.0.1" ];
        # AGH 0.107 (schema 32) listens on dns.port; bind_port is legacy and is read
        # as 0 -> defaults to 53 -> collides with dnsmasq and the unit crash-loops.
        port = 5354;
        # decision 1, public IPs for now. DoH is the half the cutover actually
        # lost (the AP had https-dns-proxy + force_dns); that is a later swap of
        # these two lists, not a redesign.
        upstream_dns = [
          "94.140.14.14"
          "1.1.1.1"
        ];
        bootstrap_dns = [ "1.1.1.1" ];
        filtering_enabled = true;
        # Phase 3: rule lists, declared rather than clicked. yaml-merge replaces
        # lists, so anything added in the UI would vanish on the next rebuild.
        # These ids are ours; don't let the UI reuse them. Nothing to migrate --
        # the e8450's AGH never sat in a DNS path.
        filters = [
          {
            enabled = true;
            id = 1;
            name = "AdGuard DNS filter";
            url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
          }
          {
            enabled = true;
            id = 2;
            name = "AdAway Default Blocklist";
            url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_2.txt";
          }
        ];
        cache_size = 4194304;
        cache_ttl_min = 300;
        cache_optimistic = true;
        querylog_enabled = true;
        querylog_file_enabled = true;
      };
      # decision 2: close the UI if loopback + ssh stops being enough. Recent AGH
      # wants a bcrypt hash here; no fork, no agenix.
      # auth.users = [ { name = "yuan"; password = "<bcrypt: htpasswd -B -n yuan>"; } ];
    };
  };

  # --- Firewall ---
  # Only the two rules that change behaviour, both copied from fw4's wan zone.
  # Everything else is already what NixOS does: input denied from WAN, masquerade,
  # forwarding accepted between the bridge and the WAN. Ping stays allowed from
  # everywhere, where fw4 only allowed it from wan.
  networking.firewall = {
    # Allow-DHCP-Renew: the WAN client's replies. Without this the lease renews
    # on .100 but never commits.
    allowedUDPPorts = [ 68 ];
    # DHCP server requests and DNS from LAN clients. NixOS's input policy drops
    # both unless they are opened, and without udp/67 dnsmasq never sees the
    # DISCOVER (the first staging boot took no lease because of exactly this).
    # Scoped to =br-lan= so the WAN side never runs a DHCP server or a resolver.
    #
    # tcp/53 is for answers over 512 bytes -- the other half of what the e8450
    # gave us for free, where fw4's =lan= zone was =input ACCEPT= and
    # https-dns-proxy's force_dns redirected every LAN query. On NixOS nothing
    # accepts 53 unless we say so, so after the swap the house resolved nothing.
    #
    # tcp/22 is ssh. It belongs here, not in sshd's own rule, for the same reason
    # as 53 and 67: that rule is global, so with eth2 lit and
    # =permitrootlogin prohibit-password= root was key-authable from the WAN link.
    interfaces."br-lan".allowedUDPPorts = [
      67
      53
    ];
    interfaces."br-lan".allowedTCPPorts = [
      53
      22
    ];
  };

  # sshd's openFirewall default is an unscoped =tcp dport 22= accept, which is
  # why 22 answered on eth2's network. Off, and the =br-lan= rule above is the
  # only way in. ttyS0 is the way back if a generation ever moves br-lan's
  # address out from under us.
  services.openssh.openFirewall = false;
}
