#!/bin/sh
# Armed by `just openwrt-apply` before it commits. Its job is to undo that commit if we
# never disarm it -- which is the signature of a config that is wrong in exactly the
# way that costs us the session: a bad input policy, a zone that drops LAN, a bridge
# that will not come up. That failure is indistinguishable from a dead box from here,
# and by the time we know the difference the connection that could have undone it is
# gone. So: sleep, and if nobody disarmed it, restore and bounce what reads the config.
#
# Disarm is the deletion of /tmp/rollback.armed, not a signal. Busybox has no `nohup`
# and its setsid forks, so a recorded PID is unreliable; a marker checked after the
# sleep is. If the router reboots, /tmp is RAM, so both the script and the marker are
# gone and nothing fires -- this is not a boot-time rollback. A box with 31 days of
# uptime is far more likely to get an unreachable LAN than to lose power mid-apply,
# and state/openwrt/e8450/live is the manual path back for that.

sleep 90

[ -e /tmp/rollback.armed ] || exit 0

tar xf /etc/rollback.tar -C /etc/config/ 2>/dev/null
for s in network firewall dnsmasq; do
	[ -x /etc/init.d/$s ] && /etc/init.d/$s reload 2>/dev/null
done
sync
