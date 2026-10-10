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

SECRET_OPTIONS = ("key", "password", "passwd", "secret")


def redact(path: pathlib.Path) -> bool:
    original = path.read_text()
    pattern = re.compile(
        r"(option (%s) ')(?!/)([^']+)(')" % "|".join(SECRET_OPTIONS)
    )
    updated = pattern.sub(
        # `$p$<user>` is the shipped rpcd reference to the user's shadow entry,
        # not a secret anyone typed, so keep it as-is.
        lambda m: m.group(0)
        if m.group(3).startswith("$p$")
        else f"{m.group(1)}@{path.name}_{m.group(2)}@{m.group(4)}",
        original,
    )
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
