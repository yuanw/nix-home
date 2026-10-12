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
