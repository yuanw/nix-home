#!/usr/bin/env python3
"""Update librewolf-macos to the latest macOS DMG from Codeberg."""

import json
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import update_srcs

API = "https://codeberg.org/api/v1/repos/librewolf/source/releases?limit=50"
PKG_BASE = "https://codeberg.org/api/packages/librewolf/generic/librewolf"
ARCH = "macos-arm64"


def curl_ok(*args: str) -> bool:
    result = subprocess.run(["curl", "-sf", *args], capture_output=True)
    return result.returncode == 0


def dmg_url(version: str) -> str:
    return f"{PKG_BASE}/{version}/librewolf-{version}-{ARCH}-package.dmg"


def get_latest_release() -> str:
    result = subprocess.run(["curl", "-sfL", API], capture_output=True, text=True, check=True)
    releases = json.loads(result.stdout)

    for release in releases:
        version = release["tag_name"].lstrip("v")
        if curl_ok("-I", dmg_url(version)):
            return version

    msg = "No macOS DMG release found"
    raise ValueError(msg)


def main() -> None:
    pkg_dir = Path(__file__).parent

    print("Fetching latest librewolf macOS release...")
    version = get_latest_release()
    print(f"Latest version: {version}")

    update_srcs(pkg_dir, version, dmg_url(version))


if __name__ == "__main__":
    main()
