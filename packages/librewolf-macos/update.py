#!/usr/bin/env python3
"""Update librewolf-macos to the latest release from Codeberg."""

import json
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import update_srcs


def get_latest_release() -> dict:
    """Fetch latest release info from the Codeberg API."""
    api_url = "https://codeberg.org/api/v1/repos/librewolf/source/releases/latest"

    with urllib.request.urlopen(api_url) as response:  # noqa: S310
        release = json.loads(response.read().decode())

    version = release["tag_name"].lstrip("v")
    return {"version": version}


def main() -> None:
    pkg_dir = Path(__file__).parent

    print("Fetching latest librewolf release...")
    release = get_latest_release()
    version = release["version"]
    print(f"Latest version: {version}")

    url = f"https://librewolf.dev/api/packages/librewolf/generic/librewolf/{version}/librewolf-{version}-macos-arm64-package.dmg"

    update_srcs(pkg_dir, version, url)


if __name__ == "__main__":
    main()
