#!/usr/bin/env python3
"""Replace secrets in a live /etc/config dump with @placeholders.

The snapshot under state/openwrt/<router>/live is both the drift baseline and the
secret map: every @name@ there is something nix-uci fills in from 1Password.
Keeping the dump git-safe is the point, because a router config *is* a secrets
file if you copy all of it.
"""

import pathlib
import re
import sys

# This is the only thing between a router config dump and the repo, so it is drawn
# wider than the four names that happen to be live today: this box runs AdGuard,
# uhttpd and rpcd, and a future dropbear or wireguard section would push a real key
# through the same pipe. `openwrt-facts` is the only writer and runs it unconditionally.
SECRET_OPTIONS = frozenset(
    ("key", "password", "passwd", "secret", "psk", "passphrase", "token", "privkey")
)

OPTION_LINE = re.compile(r"(option|list) ([A-Za-z_][A-Za-z0-9_]*) '([^']*)'")


def redact(path: pathlib.Path) -> bool:
    original = path.read_text()

    def substitute(match: re.Match) -> str:
        keyword, name, value = match.group(1).lower(), match.group(2).lower(), match.group(3)
        if name not in SECRET_OPTIONS:
            return match.group(0)
        # `$p$<user>` is the shipped rpcd reference to the user's shadow entry, and a
        # leading / is a path, so neither is a secret anyone typed. A value already
        # holding a placeholder is left alone, because redaction runs on every facts.
        if value.startswith(("$p$", "/", "@")):
            return match.group(0)
        return f"{keyword} '{name}' '@{path.name}_{name}@'"

    updated = OPTION_LINE.sub(substitute, original)
    if updated != original:
        path.write_text(updated)
        return True
    return False


def main() -> int:
    if len(sys.argv) != 2:
        print(f"USAGE: {sys.argv[0]} CONFIG_DIR", file=sys.stderr)
        return 2
    changed = [p.name for p in pathlib.Path(sys.argv[1]).glob("*") if p.is_file() and redact(p)]
    if changed:
        print(f"redacted: {', '.join(changed)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
