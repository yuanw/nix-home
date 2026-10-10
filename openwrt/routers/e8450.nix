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
  };
}
