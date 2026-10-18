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

  # Both ASK fixes this box needed from upstream live in the fork now: the
  # ctnetlink ABI fix (libnetfilter_conntrack sized CTA_COMCERTO_FP_XFRM_HANDLE
  # as 16 bytes while the kernel sends 8, so cmm crash-looped and the fast path
  # never got programmed) and the option below. See the flake input.
  mono-gateway.ask.ipsecOffload = false;

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
  networking.interfaces.eth2 = {
    useDHCP = true;
    macAddress = "80:69:1a:1f:28:de";
  };

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
  # on the WAN side too. Two things changed since this block was copied from the
  # e8450: decision 4 set ~no-resolv = true~ so the WAN's DHCP-provided resolvers
  # in /etc/resolv.conf are never read, and since 2026-10-18 dnsmasq is the last
  # hop rather than the first -- its ~server= list below is the only way out.
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

      # Decision 4, closed 2026-10-17: do not import /etc/resolv.conf as upstream
      # servers. Without it dnsmasq still held the WAN resolvers, so a dead AGH
      # meant *unfiltered* resolution -- and the fallback is not hypothetical: a
      # 12s stop during testing let real traffic out through the ONT, and the
      # cached answers kept serving after AGH came back. What survives this key is
      # decision 4's guarantee: with the ONT path shut off, the only way out is
      # through a service that filters. Since 2026-10-18 the guarantee is stronger
      # than it was -- dnsmasq answers only AGH -- and so is the failure: either
      # service going down takes external resolution, and dnsmasq alone takes
      # =.lan= and =*.yuanw.me= with it. The lever back is
      # ~just mono-gateway-rollback~, and =dnsmasq --test= in preStart fails
      # activation rather than the house if this key ever loses its upstream.
      no-resolv = true;

      # Last hop now, not the front door: AGH owns :53 and asks dnsmasq on 5354.
      # The port is what makes the swap possible. Measured here with a throwaway
      # instance: =bind-interfaces= + =interface = br-lan= binds =192.168.1.1:53=
      # whatever =listen-address= says, so dnsmasq cannot be pushed onto loopback
      # and stay on :53 -- it would only win the port from AGH. On 5354 no LAN
      # client can reach it: =chain input= is =policy drop= and no rule opens it.
      # shortcut: "unreachable from the LAN" is firewall default here, not a bind.
      # Give 5354 an accept rule and dnsmasq becomes a resolver the house can use
      # around the filter.
      port = 5354;

      # The external path is dnsmasq's decision now, not AGH's. The same public
      # pair AGH had, so decision 1 keeps its meaning (plaintext IPv4, still), and
      # =no-resolv = true= keeps decision 4 shut: the ONT resolvers in
      # /etc/resolv.conf are never read, so nothing leaves for the WAN except by
      # way of AGH. 2.91 has no DoT/DoH upstream, so moving decision 1 is an
      # unbound/proxy job in front of this key, not a change to it.
      #
      # The catch-all that pointed back at AGH is gone -- it would be a loop now
      # that AGH stands upstream of dnsmasq. Deleting it is the whole cycle check;
      # =dnsmasq --test= in preStart still fails activation rather than the house.
      server = [
        "94.140.14.14"
        "1.1.1.1"
      ];
    };
  };

  # AGH has to release 5354 before dnsmasq can take it, so dnsmasq goes after it.
  # The retry is the load-bearing part: dnsmasq failed on "address already in use"
  # is the whole house offline -- local names, external names and DHCP are all in
  # that one unit now.
  systemd.services.dnsmasq = {
    after = [ "adguardhome.service" ];
    serviceConfig.RestartSec = 2;
    serviceConfig.StartLimitIntervalSec = 0;
  };

  # --- AdGuard Home (front door since 2026-10-18) ---
  # AGH answers the LAN on :53 and dnsmasq is what it asks. Clients still
  # configure nothing: option 6 in DHCP is still 192.168.1.1, and only the port
  # moved.
  #
  # AGH has exactly ONE upstream list on this binary -- probed: no
  # domain-specific-upstream keys and no =local_dns_hosts=, both of which are 1.x
  # -- so that list has to be dnsmasq. That is what keeps leases, =.lan= names and
  # =*.yuanw.me= answering with nothing rebuilt. The old decision against owning
  # :53 counted on the 1.x escape hatch; this is the swap that works without it.
  #
  # host/port are service options, NOT settings.bind_host -- current nixpkgs
  # asserts bind_host away, and host's default is 0.0.0.0, so declaring it *is*
  # the exposure decision. Decision 2 moved the *UI* to br-lan so misfit's caddy
  # can serve it as https://agh.yuanw.me (modules/caddy.nix), gated by the login
  # below rather than by a loopback bind. eth2 is still shut: input is dropped
  # from the WAN side and every accept rule here names an interface. Note that
  # binding br-lan stops loopback from answering 3000, so the tunnel form is
  #   ssh -N -L 3000:192.168.1.1:3000 root@192.168.1.1
  services.adguardhome = {
    enable = true;
    openFirewall = false;
    host = "192.168.1.1";
    port = 3000;
    settings = {
      dns = {
        # Front door: =192.168.1.1:53=, which the firewall already accepts from
        # br-lan on udp and tcp. =::= would put a resolver on eth2, which is what
        # =bind_hosts= is for.
        bind_hosts = [ "192.168.1.1" ];
        # AGH 0.107 (schema 32) listens on dns.port; bind_port is legacy and is
        # read as 0 -> defaults to 53 -> collides and the unit crash-loops.
        port = 53;
        # dnsmasq, not a public resolver -- see the single-upstream note above.
        # The public pair now lives in =services.dnsmasq.settings.server=, and
        # decision 1 moved there with it.
        upstream_dns = [ "127.0.0.1:5354" ];
        filtering_enabled = true;
        cache_size = 4194304;
        cache_ttl_min = 300;
        cache_optimistic = true;
      };

      # Phase 3: rule lists, declared rather than clicked. yaml-merge replaces
      # lists, so anything added in the UI would vanish on the next rebuild, and
      # these ids are ours -- don't let the UI reuse them. Nothing to migrate,
      # the e8450's AGH never sat in a DNS path.
      #
      # TOP LEVEL, not under dns -- that is the part every Nix example gets
      # wrong. In schema 32 / AGH 0.107.71 `filters` sits next to `dns`,
      # alongside whitelist_filters and user_rules; a list declared inside
      # settings.dns is parsed away and AGH silently falls back to its own
      # compiled-in default pair (AdGuard DNS filter + AdAway, the second
      # disabled). That was the real cause of the Phase 3 "AdAway refuses to
      # load" mystery, which this commit's predecessor blamed on the version.
      # Verified on dgx-spark: top-level -> 185868 + 6540 + 13085 rules; under
      # dns -> nothing but the defaults, on a clean state dir, twice.
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

      # TOP LEVEL, for the same reason as filters and by the same mechanism: at
      # schema 32 the query-log keys live in a `querylog:` map next to `dns:`
      # (state yaml: enabled, file_enabled, interval, size_memory, ignored,
      # dir_path), and the flat `querylog_*` names exist in the 0.107.71 binary
      # only on the schema-migration path. Declared under `dns` they are parsed
      # away, so the log ran on AGH's own defaults and not on this declaration --
      # which made the docs' "querylog_enabled = false" mitigation a no-op. The
      # rest stays at AGH's defaults: size_memory 1000 (the in-memory ring) and
      # interval 2160h, which is this binary's only retention knob -- 0.107.71 has
      # no `retention` key (grep'd: yaml:"interval" yes, yaml:"retention" no). At
      # the measured ~26 MB of querylog.json a day, 90 days is ~2.3 GB on 27 GB
      # free, so bound `interval` here rather than discovering it later.
      querylog = {
        enabled = true;
        file_enabled = true;
      };

      # Decision 2, closed: the UI answers on br-lan now, so it needs a login.
      # bcrypt hash only -- the password lives in pass:monogateway/agh/password
      # and in nothing else. After a rotation, regenerate the hash with:
      #   pass show monogateway/agh/password | htpasswd -nBi -C 10 yuan | cut -d: -f2
      #
      # KEY NAME, measured the hard way: in 0.107.71 the list is a TOP-LEVEL
      # =users= key, not =auth.users= -- that block is 1.x. Written the newer way
      # it is parsed away exactly like =filters= was: --check-config still says
      # "configuration file is ok", the unit still logs "no users in the
      # configuration file; authentication is disabled", and the UI still answers
      # 200 to anonymous /control/status. So this is the one shape that works on
      # this binary; re-check it on any adguardhome bump. The rest of the block
      # (http.session_ttl at 720h, auth_attempts, block_auth_min) stays at its
      # defaults -- session_ttl is deliberately not declared here because the
      # =http= map also holds the bind address the module derives from =host=.
      #
      # Consequence: every /control endpoint needs a session now, so the curl
      # checks in this repo's runbooks need a login first.
      users = [
        {
          name = "yuan";
          password = "$2y$10$Yq7UPL6vtEJGxPqRX75W6O/o4F/cGk5C5br9oRO0F2DUPUO0eJ55C";
        }
      ];
    };
  };

  # --- Firewall ---
  # Three rules change behaviour now, two of them copied from fw4's wan zone.
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
      # The AGH UI. It is the one port here opened for a service rather than for
      # the LAN's own traffic, and it is gated by =auth= instead of by a bind --
      # any LAN host can reach the login page, and reaching it over plain http is
      # still possible. Tightening it to 192.168.1.190 alone needs an nft rule of
      # our own; there is no precedent for that in this repo yet.
      3000
    ];
  };

  # sshd's openFirewall default is an unscoped =tcp dport 22= accept, which is
  # why 22 answered on eth2's network. Off, and the =br-lan= rule above is the
  # only way in. ttyS0 is the way back if a generation ever moves br-lan's
  # address out from under us.
  services.openssh.openFirewall = false;
}
