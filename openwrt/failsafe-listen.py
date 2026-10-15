#!/usr/bin/env python3
"""Catch OpenWrt's failsafe "press the button" broadcast.

OpenWrt's preinit re-broadcasts "Please press button now to enter failsafe" to
<broadcast>:4919 once a second for the whole 4 s window (see
docs/openwrt-plan.org, *Failsafe rehearsal*). That packet is the only reliable
timing oracle for the WPS press: the window is too short to guess, and the LEDs a
few cm away are the wrong clock. Receiving it needs no root (unlike
tcpdump/BPF) -- just a UDP socket.

Run it on the machine wired to the router's LAN1 (the only port failsafe answers
on), reboot the router, and press WPS when it prints. No gateway or DNS needed;
a static 192.168.1.10/24 on the wired NIC is enough.

    python3 openwrt/failsafe-listen.py            # print + terminal bell
    python3 openwrt/failsafe-listen.py --speak    # also say it (macOS `say`)

busybox's netmsg frames the payload as 0xdead + text + NUL padding, so the
message is decoded leniently.
"""

import argparse
import socket
import subprocess
import sys

PORT = 4919
NEEDLE = "press button"
FRAME = b"\xde\xad"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--port", type=int, default=PORT)
    ap.add_argument(
        "--speak",
        action="store_true",
        help="speak the prompt (`say`), for when the screen is out of sight of the button",
    )
    ap.add_argument(
        "--timeout",
        type=float,
        default=None,
        help="exit after N seconds of silence (default: run until Ctrl-C)",
    )
    args = ap.parse_args()

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
    try:
        sock.bind(("0.0.0.0", args.port))
    except OSError as exc:
        print(f"cannot bind udp/{args.port}: {exc}", file=sys.stderr)
        return 1
    if args.timeout:
        sock.settimeout(args.timeout)

    print(f"listening udp/{args.port} for the OpenWrt failsafe prompt …", flush=True)
    try:
        while True:
            data, addr = sock.recvfrom(2048)
            payload = data[len(FRAME) :] if data.startswith(FRAME) else data
            msg = payload.decode(errors="replace").strip("\x00").strip()
            print(f"\a{addr[0]}: {msg}", flush=True)
            if NEEDLE in msg.lower():
                print(">>> PRESS WPS NOW <<<", flush=True)
                if args.speak:
                    subprocess.Popen(["say", "press the WPS button now"])
    except KeyboardInterrupt:
        return 0
    except socket.timeout:
        print(
            f"no prompt within {args.timeout:g}s -- press missed or window closed",
            file=sys.stderr,
        )
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
