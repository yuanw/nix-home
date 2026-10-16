# The e8450 as a *dumb AP*, for cutover step 2: the Mono Gateway becomes the
# router (192.168.1.1) and this box only bridges its LAN ports and radios.
#
# It imports the router declaration and overrides only the parts that make it a
# router, so the two cannot drift: change something shared in e8450.nix and both
# targets pick it up. Deliberately a separate target, not an edit to e8450.nix --
# the cutover's whole back-out is "re-cable to the e8450 and it is exactly as
# declared", and that only holds while e8450.nix stays the router config.
#
# What changes, and nothing else:
#   - network.lan: .1 -> .2 (the board takes .1), and drop the /60 PD assign.
#     It keeps a gateway/DNS at .1 so the AP's own NTP/opkg still work.
#   - dhcp.lan: no server, no RAs -- the board is the authority now.
#   - network.wan/wan6: proto=none -- the ISP drop moved to the board.
#
# Not changed on purpose (flagged, not silently decided): AdGuardHome + the
# dnsmasq forwarding chain still run on this box. The cutover design has not yet
# said whether the LAN's adblocking moves to the board; until it does, the AP
# keeps serving DNS on .2 (unused by clients, which get .1 from the board).
{ lib, ... }:
{
  imports = [ ./e8450.nix ];

  uci.settings = {
    network.lan = lib.mkForce {
      _type = "interface";
      device = "br-lan";
      proto = "static";
      ipaddr = "192.168.1.2";
      netmask = "255.255.255.0";
      # the AP is a client of the board for its own traffic (NTP, opkg)
      gateway = "192.168.1.1";
      dns = [ "192.168.1.1" ];
    };

    dhcp.lan = lib.mkForce {
      _type = "dhcp";
      interface = "lan";
      ignore = "1";
      ra = "disabled";
      dhcpv6 = "disabled";
    };

    network.wan = lib.mkForce {
      _type = "interface";
      device = "wan";
      proto = "none";
    };

    network.wan6 = lib.mkForce {
      _type = "interface";
      device = "wan";
      proto = "none";
    };
  };
}
