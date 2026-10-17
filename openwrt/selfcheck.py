#!/usr/bin/env nix-shell
#!/nix-shell -i python3 -p python3
"""One runnable check for the serialization rules.

    nix run nixpkgs#python3 -- openwrt/selfcheck.py
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import nix_uci  # noqa: E402

SECRETS = {"wifi_key": "sekret"}

named = nix_uci.serialize_named("wireless", "default_radio0", {
    "_type": "wifi-iface",
    "ssid": "NoneAndVoid",
    "key": "@wifi_key@",
}, SECRETS)
assert named == [
    "delete wireless.default_radio0",
    "set wireless.default_radio0=wifi-iface",
    "set wireless.default_radio0.ssid='NoneAndVoid'",
    "set wireless.default_radio0.key='sekret'",
], named

anon = nix_uci.serialize({"system": {"system": [{"_type": "system", "hostname": "e8450"}]}}, SECRETS)
# The generator must not delete anonymous sections itself (the caller wipes the
# type first, unbounded); `add` lines are what tell it which types to wipe.
assert not any(line.startswith("delete ") for line in anon.splitlines()), anon
assert anon.splitlines().count("add system system") == 1, anon
assert "add system system" in anon, anon
assert "set system.@system[0].hostname='e8450'" in anon, anon

lists = nix_uci.serialize({"system": {"ntp": {
    "_type": "timeserver", "server": ["a", "b"],
}}}, SECRETS)
assert "add_list system.ntp.server='a'" in lists, lists
assert "add_list system.ntp.server='b'" in lists, lists

# A bool has to reach the router as 1/0; 'True' is not something OpenWrt init
# scripts read as enabled, and every value in e8450.nix being a string today is no
# reason for the next one to be safe.
bools = nix_uci.serialize({"a": {"s": {"_type": "t", "x": True, "y": False}}}, SECRETS)
assert "set a.s.x='1'" in bools and "set a.s.y='0'" in bools, bools

# uci batch has no quoting escape, so a value containing ' would silently truncate
# the command -- a half-applied config, which is worse than a failed build.
try:
    nix_uci.serialize({"a": {"s": {"_type": "t", "x": "it's"}}}, SECRETS)
except nix_uci.ConfigError:
    pass
else:
    raise AssertionError("a value uci cannot quote should have raised ConfigError")

must_raise = [
    ("undefined secret", lambda: nix_uci.interpolate("@missing@", {})),
    ("empty secret", lambda: nix_uci.interpolate("@empty@", {"empty": ""})),
    ("section without _type", lambda: nix_uci.serialize({"a": {"b": {"x": 1}}}, SECRETS)),
    ("config that is not a dict", lambda: nix_uci.serialize("nope", SECRETS)),
]
for label, call in must_raise:
    try:
        call()
    except nix_uci.ConfigError:
        continue
    raise AssertionError(f"{label} should have raised ConfigError")

print("nix-uci selfcheck: ok")

# raw: appended verbatim after the declared config, verbs whitelisted so a stray
# shell command or a `commit` in the config is a build error, not a surprise.
import json as _json, tempfile, os
_doc = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False)
_json.dump({"settings": {"system": {"system": [{"_type": "system", "hostname": "h"}]}},
           "raw": ["reorder firewall.@rule[0]=2"]}, _doc)
_doc.flush()
out = nix_uci.convert(_doc.name, SECRETS).splitlines()
assert out[-1] == "reorder firewall.@rule[0]=2", out
assert out.index("add system system") < len(out) - 1, out
try:
    _bad = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False)
    _json.dump({"settings": {"system": {"system": [{"_type": "system", "hostname": "h"}]}},
               "raw": ["rm -rf /etc/config"]}, _bad)
    _bad.flush()
    nix_uci.convert(_bad.name, SECRETS)
except nix_uci.ConfigError:
    pass
else:
    raise AssertionError("raw accepted a non-uci command")
os.unlink(_doc.name); os.unlink(_bad.name)

# --- redact-config: the wall between /etc/config and a public repo ------------
# It fails in the check rather than on the box, like everything else here. Run as
# openwrt-facts runs it: a directory of live configs, in place, plus an exit code.
import pathlib
import subprocess
import tempfile

_here = os.path.dirname(os.path.abspath(__file__))


def run_redactor(files: dict) -> tuple:
    outdir = pathlib.Path(tempfile.mkdtemp())
    for name, body in files.items():
        (outdir / name).write_text(body)
    proc = subprocess.run(
        [sys.executable, os.path.join(_here, "redact-config.py"), str(outdir)],
        capture_output=True,
        text=True,
    )
    return proc.returncode, {name: (outdir / name).read_text() for name in files}, proc.stderr


# A declared name is redacted however long it is, and that is the path nix-uci
# fills back in from `pass`.
rc, got, _ = run_redactor({"wireless": "option ssid 'NoneAndVoid'\noption key '0123456789abcdef'\n"})
assert rc == 0, (rc, got)
assert "option ssid 'NoneAndVoid'" in got["wireless"], got
assert "option 'key' '@wireless_key@'" in got["wireless"], got

# An undeclared name that only *looks* secret must not be stopped or rewritten:
# key_type is config, not a key, and stopping the snapshot over it is a false alarm.
rc, got, _ = run_redactor({"uhttpd": "option key_type 'ec'\noption key '/etc/uhttpd.key'\n"})
assert rc == 0, (rc, got, "key_type must not trip the net")
assert "option key_type 'ec'" in got["uhttpd"], got
assert "option key '/etc/uhttpd.key'" in got["uhttpd"], got

# An undeclared name that *is* holding key material has to be redacted and has to
# fail the run. This is the case the eight-name list cannot see coming.
secret = "aB3dEf6gHi9jKl2mNo"
rc, got, err = run_redactor({"ipsec": f"option presharedkey '{secret}'\n"})
assert rc == 1, (rc, "an undeclared secret option must exit non-zero")
assert "option 'presharedkey' '@ipsec_presharedkey@'" in got["ipsec"], got
assert secret not in str(got.values()), "the secret value is still in the tree"
assert "ipsec.presharedkey" in err, err

# The rpcd reference into /etc/shadow and an already-redacted value stay put, so
# re-running facts on a snapshot does not chew through its own placeholders.
rc, got, _ = run_redactor({"rpcd": "option password '$p$root'\noption key '@rpcd_key@'\n"})
assert rc == 0, (rc, got)
assert got["rpcd"] == "option password '$p$root'\noption key '@rpcd_key@'\n", got

print("redact-config selfcheck: ok")

