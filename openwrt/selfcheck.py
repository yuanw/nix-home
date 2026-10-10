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
