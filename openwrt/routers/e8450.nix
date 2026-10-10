# Declared state for the Linksys E8450 router (192.168.1.1).
#
# Translated 1:1 from state/openwrt/e8450/config.txt, so a first `just
# openwrt-diff` should come back empty.  Change values here to change the router;
# sections absent here are left untouched, because nix-uci only deletes the
# sections it is told about.
{
  uci.settings = {
    # Anonymous section (`config system` with no name), so this is a list whose
    # attribute name is the section type.
    system.system = [
      {
        _type = "system";
        hostname = "OpenWrt";
        # Same wall clock as every other host here (UTC-6, no DST), in POSIX form
        # because this image has no /usr/share/zoneinfo. Once packages are declared,
        # switch to "America/Regina" and apk add zoneinfo-america.
        timezone = "CST6";
        ttylogin = "0";
        log_size = "128";
        urandom_seed = "0";
        compat_version = "2.0";
      }
    ];

    system.ntp = {
      _type = "timeserver";
      enabled = "1";
      enable_server = "0";
      server = [
        "0.openwrt.pool.ntp.org"
        "1.openwrt.pool.ntp.org"
        "2.openwrt.pool.ntp.org"
        "3.openwrt.pool.ntp.org"
      ];
    };

    system.led_wan = {
      _type = "led";
      name = "WAN";
      sysfs = "inet:blue";
      trigger = "netdev";
      mode = "link tx rx";
      dev = "wan";
    };

    dropbear.main = {
      _type = "dropbear";
      PasswordAuth = "off";
      RootPasswordAuth = "off";
      Port = "22";
    };

    wireless.radio0 = {
      _type = "wifi-device";
      type = "mac80211";
      phy = "wl0";
      cell_density = "0";
      country = "CA";
      band = "2g";
      channel = "6";
      htmode = "HT20";
    };

    wireless.default_radio0 = {
      _type = "wifi-iface";
      device = "radio0";
      network = "lan";
      mode = "ap";
      ssid = "NoneAndVoid";
      encryption = "psk2";
      key = "@wireless_key@";
    };

    wireless.radio1 = {
      _type = "wifi-device";
      type = "mac80211";
      phy = "wl1";
      band = "5g";
      channel = "36";
      htmode = "HE80";
      disabled = "0";
    };

    wireless.default_radio1 = {
      _type = "wifi-iface";
      device = "radio1";
      network = "lan";
      mode = "ap";
      ssid = "NoneAndVoid-5G";
      encryption = "psk2";
      key = "@wireless_key@";
    };
    # --- everything else, translated 1:1 from state/openwrt/e8450/live ------------
    # Declared so drift shows up as a diff instead of disappearing. None of these
    # can lock us out: worst case is a service restart. `network` is declared too --
    # see its block below -- and `firewall` is still absent, waiting on the sorted
    # diff. See docs/openwrt-plan.org, "the lockout-risk configs".

    adguardhome.config = {
      _type = "adguardhome";
      workdir = "/var/adguardhome";
      config_file = "/etc/adguardhome/adguardhome.yaml";
    };

    attendedsysupgrade.server = {
      _type = "server";
      url = "https://sysupgrade.openwrt.org";
    };

    attendedsysupgrade.client = {
      _type = "client";
      upgrade_packages = "1";
      auto_search = "0";
      advanced_mode = "0";
      login_check_for_upgrades = "1";
    };

    ubihealthd.ubi0 = {
      _type = "ubi-device";
      device = "/dev/ubi0";
      enable = "1";
    };

    # Both anonymous, both same type -- the list is the section order.
    ubootenv.ubootenv = [
      {
        _type = "ubootenv";
        dev = "/dev/ubi0_2";
        offset = "0x0";
        envsize = "0x1f000";
        secsize = "0x1f000";
        numsec = "1";
      }
      {
        _type = "ubootenv";
        dev = "/dev/ubi0_3";
        offset = "0x0";
        envsize = "0x1f000";
        secsize = "0x1f000";
        numsec = "1";
      }
    ];

    rpcd.rpcd = [
      {
        _type = "rpcd";
        socket = "/var/run/ubus/ubus.sock";
        timeout = "30";
      }
    ];

    # `$p$root` is shipped this way: rpcd authenticates against root's shadow entry.
    rpcd.login = [
      {
        _type = "login";
        username = "root";
        password = "$p$root";
        read = [ "*" ];
        write = [ "*" ];
      }
    ];

    https-dns-proxy.config = {
      _type = "main";
      canary_domains_icloud = "1";
      canary_domains_mozilla = "1";
      dnsmasq_config_update = "*";
      force_dns = "1";
      force_dns_port = [
        "53"
        "853"
      ];
      procd_trigger_wan6 = "0";
    };

    https-dns-proxy.dns = {
      _type = "https-dns-proxy";
      bootstrap_dns = "1.1.1.1";
      resolver_url = "https://cloudflare-dns.com/dns-query";
      listen_addr = "127.0.0.1";
      listen_port = "5053";
    };

    uhttpd.main = {
      _type = "uhttpd";
      listen_http = [
        "0.0.0.0:80"
        "[::]:80"
      ];
      listen_https = [
        "0.0.0.0:443"
        "[::]:443"
      ];
      redirect_https = "0";
      home = "/www";
      rfc1918_filter = "1";
      max_requests = "3";
      max_connections = "100";
      cert = "/etc/uhttpd.crt";
      key = "/etc/uhttpd.key";
      cgi_prefix = "/cgi-bin";
      lua_prefix = [ "/cgi-bin/luci=/usr/lib/lua/luci/sgi/uhttpd.lua" ];
      script_timeout = "60";
      network_timeout = "30";
      http_keepalive = "20";
      tcp_keepalive = "1";
      ubus_prefix = "/ubus";
    };

    uhttpd.defaults = {
      _type = "cert";
      days = "397";
      key_type = "ec";
      bits = "2048";
      ec_curve = "P-256";
      country = "ZZ";
      state = "Somewhere";
      location = "Unknown";
      commonname = "OpenWrt";
    };

    luci.main = {
      _type = "core";
      lang = "auto";
      mediaurlbase = "/luci-static/bootstrap";
      resourcebase = "/luci-static/resources";
      ubuspath = "/ubus/";
    };

    luci.flash_keep = {
      _type = "extern";
      uci = "/etc/config/";
      dropbear = "/etc/dropbear/";
      openvpn = "/etc/openvpn/";
      passwd = "/etc/passwd";
      opkg = "/etc/opkg.conf";
      firewall = "/etc/firewall.user";
      uploads = "/lib/uci/upload/";
    };

    # Empty section: it exists so LuCI has a place to write languages.
    luci.languages = { _type = "internal"; };

    luci.sauth = {
      _type = "internal";
      sessionpath = "/tmp/luci-sessions";
      sessiontime = "3600";
    };

    luci.ccache = {
      _type = "internal";
      enable = "1";
    };

    luci.themes = {
      _type = "internal";
      Bootstrap = "/luci-static/bootstrap";
      BootstrapDark = "/luci-static/bootstrap-dark";
      BootstrapLight = "/luci-static/bootstrap-light";
    };

    luci.apply = {
      _type = "internal";
      rollback = "90";
      holdoff = "4";
      timeout = "5";
      display = "1.5";
    };

    luci.diag = {
      _type = "internal";
      dns = "openwrt.org";
      ping = "openwrt.org";
      route = "openwrt.org";
    };

    dhcp.dnsmasq = [
      {
        _type = "dnsmasq";
        domainneeded = "1";
        localise_queries = "1";
        rebind_protection = "1";
        rebind_localhost = "1";
        local = "/lan/";
        domain = "lan";
        expandhosts = "1";
        cachesize = "1000";
        authoritative = "1";
        readethers = "1";
        leasefile = "/tmp/dhcp.leases";
        localservice = "1";
        ednspacket_max = "1232";
        server = [
          "127.0.0.1#5054"
          "/mask.icloud.com/"
          "/mask-h2.icloud.com/"
          "/use-application-dns.net/"
          "127.0.0.1#5053"
        ];
        doh_backup_noresolv = "-1";
        noresolv = "1";
        doh_backup_server = [
          "127.0.0.1#5054"
          "/mask.icloud.com/"
          "/mask-h2.icloud.com/"
          "/use-application-dns.net/"
          "127.0.0.1#5053"
        ];
        doh_server = [ "127.0.0.1#5053" ];
        # leading space is real, it is how the icloud canary override is written.
        address = [
          " /router.local/router.lan/192.168.1.1"
          "/.yuanw.me/192.168.1.190"
        ];
      }
    ];

    dhcp.lan = {
      _type = "dhcp";
      interface = "lan";
      start = "100";
      limit = "150";
      leasetime = "12h";
      dhcpv4 = "server";
      dhcpv6 = "server";
      ra = "server";
      ra_slaac = "1";
      ra_flags = [
        "managed-config"
        "other-config"
      ];
    };

    dhcp.wan = {
      _type = "dhcp";
      interface = "wan";
      ignore = "1";
    };

    dhcp.odhcpd = {
      _type = "odhcpd";
      leasefile = "/tmp/odhcpd.leases";
      leasetrigger = "/usr/sbin/odhcpd-update";
      loglevel = "4";
      piodir = "/tmp/odhcpd-piodir";
      hostsdir = "/tmp/hosts";
    };

    # Anonymous, in file order. Static leases, so this is where a new machine's
    # reservation belongs.
    dhcp.host = [
      {
        _type = "host";
        name = "Mac";
        ip = "192.168.1.212";
        mac = [ "DA:E4:6A:44:49:09" ];
      }
      {
        _type = "host";
        name = "dgx-spark";
        duid = [ "00010001319cd0c54cbb47298169%47298169" ];
        mac = [ "4c:bb:47:29:81:69" ];
        hostid = "86a";
      }
      {
        _type = "host";
        name = "misfit";
        mac = [ "58:47:CA:7E:C0:05" ];
        ip = "192.168.1.190";
      }
    ];

    # Declared so drift is visible. Nothing here is applied by a routine run: a
    # `network` reload rebuilds interfaces and can drop the ssh session, so this
    # config gets changed by an explicit, confirmed apply only. Translated 1:1 from
    # the live config, so the declaration itself changes nothing.
    network.loopback = {
      _type = "interface";
      device = "lo";
      proto = "static";
      ipaddr = "127.0.0.1";
      netmask = "255.0.0.0";
    };

    network.globals = {
      _type = "globals";
      ula_prefix = "fd1f:26e2:8ef1::/48";
      # Generated once; if the router re-derives it the diff says so.
      dhcp_default_duid = "00045bc0b77d5e4a4217aca2cda7dd259d38";
    };

    # Anonymous `config device`, hence a list. This is the LAN bridge -- the part of
    # the config that can actually cut the session, because netifd tears br-lan down
    # and rebuilds it.
    network.device = [
      {
        _type = "device";
        name = "br-lan";
        type = "bridge";
        ports = [ "lan1" "lan2" "lan3" "lan4" ];
      }
    ];

    network.lan = {
      _type = "interface";
      device = "br-lan";
      proto = "static";
      ipaddr = "192.168.1.1";
      netmask = "255.255.255.0";
      ip6assign = "60";
    };

    network.wan = {
      _type = "interface";
      device = "wan";
      proto = "dhcp";
    };

    network.wan6 = {
      _type = "interface";
      device = "wan";
      proto = "dhcpv6";
    };

  };
}
