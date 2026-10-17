#!/usr/bin/env python3
"""Replace secrets in a live /etc/config dump with @placeholders.

The snapshot under openwrt/state/<router>/live is both the drift baseline and the
secret map: every @name@ there is something nix-uci fills in from `pass`. Keeping
the dump git-safe is the point, because a router config *is* a secrets file if you
copy all of it. And a name the list below does not know is not treated as safe
just because nobody wrote it: that case fails the run. See SECRET_HINTS.
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

# The net that catches what the exact list above cannot: an option named like a
# secret by a package nobody has configured yet. An ipsec `presharedkey` or a
# wireguard `client_secret` is invisible to a list of eight names, so without this
# it would be published and stay published. Redaction is not the fix here -- the
# *stop* is. A guessed placeholder is safe to commit and wrong forever, so the run
# fails and the name has to come here.
SECRET_HINTS = ("key", "secret", "token", "psk", "pass", "credential")


def looks_secret(name: str) -> bool:
    return any(hint in name for hint in SECRET_HINTS)


def redact(path: pathlib.Path) -> tuple[bool, list[str]]:
    """Redact one config file; return (changed, options that looked secret but are undeclared)."""
    original = path.read_text()
    suspects: list[str] = []

    def substitute(match: re.Match) -> str:
        keyword, name, value = match.group(1).lower(), match.group(2).lower(), match.group(3)
        declared = name in SECRET_OPTIONS
        if not (declared or looks_secret(name)):
            return match.group(0)
        # `$p$<user>` is the shipped rpcd reference to the user's shadow entry, and a
        # leading / is a path, so neither is a secret anyone typed. A value already
        # holding a placeholder is left alone, because redaction runs on every facts.
        if value.startswith(("$p$", "/", "@")):
            return match.group(0)
        # An undeclared name is only worth stopping over if the value reads like key
        # material. This is what keeps `option key_type 'ec'` out of the suspects.
        # shortcut: a real secret of 8 chars or fewer behind an *undeclared* name
        # slips through; declare the name and it is redacted by rule instead.
        if not declared and len(value) <= 8:
            return match.group(0)
        if not declared:
            suspects.append(name)
        return f"{keyword} '{name}' '@{path.name}_{name}@'"

    updated = OPTION_LINE.sub(substitute, original)
    if updated != original:
        path.write_text(updated)
        return True, sorted(set(suspects))
    return False, []


def main() -> int:
    if len(sys.argv) != 2:
        print(f"USAGE: {sys.argv[0]} CONFIG_DIR", file=sys.stderr)
        return 2
    changed, suspects = [], []
    for path in sorted(pathlib.Path(sys.argv[1]).glob("*")):
        if not path.is_file():
            continue
        hit, found = redact(path)
        if hit:
            changed.append(path.name)
        suspects += [f"{path.name}.{name}" for name in found]
    if changed:
        print(f"redacted: {', '.join(changed)}")
    if suspects:
        print(
            f"UNDECLARED SECRET OPTIONS: {', '.join(suspects)}\n"
            "  redacted, but nothing knows these are secrets -- add the names to\n"
            "  SECRET_OPTIONS in openwrt/redact-config.py and re-run `just openwrt-facts`\n"
            "  before committing this snapshot.",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
