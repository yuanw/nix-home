#!/usr/bin/env python3
"""Filter GitHub notifications: drop closed/merged PRs, log and dismiss via gh.

Reads notifications JSON from stdin, writes filtered JSON to stdout.

Env:
  GITHUB_NOTIFICATIONS_LOG_DIR  log directory (default: ~/.local/state/sketchybar)
  GITHUB_NOTIFICATIONS_DISMISS  dismiss closed/merged PRs (default: 1)
  GITHUB_NOTIFICATIONS_DRY_RUN  log only, do not PATCH (default: 0)
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path


def env_bool(name: str, default: bool) -> bool:
    value = os.environ.get(name)
    if value is None:
        return default
    return value not in {"0", "false", "False", "no", "NO"}


def gh_get(endpoint: str) -> dict | None:
    try:
        result = subprocess.run(
            ["gh", "api", endpoint],
            check=True,
            capture_output=True,
            text=True,
        )
        return json.loads(result.stdout)
    except (subprocess.CalledProcessError, json.JSONDecodeError):
        return None


def gh_mark_read(thread_url: str) -> None:
    try:
        subprocess.run(
            ["gh", "api", "-X", "PATCH", thread_url, "-f", "read=true"],
            check=True,
            capture_output=True,
            text=True,
        )
    except subprocess.CalledProcessError:
        pass


def process_notification(
    notification: dict,
    *,
    dismiss: bool,
    dry_run: bool,
) -> tuple[dict | None, dict | None]:
    subject = notification.get("subject", {})
    if subject.get("type") != "PullRequest":
        return notification, None

    pr = gh_get(subject["url"]) or {}
    state = pr.get("state", "unknown")
    if state == "open":
        return notification, None

    log_entry = {
        "timestamp": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "action": "dismiss",
        "repo": notification.get("repository", {}).get("full_name", ""),
        "title": subject.get("title", ""),
        "notification_reason": notification.get("reason", ""),
        "pr_state": state,
        "merged": bool(pr.get("merged_at")),
        "thread_url": notification.get("url", ""),
        "pr_url": pr.get("html_url", ""),
    }

    if dismiss and not dry_run:
        gh_mark_read(notification["url"])

    return None, log_entry


def main() -> int:
    log_dir = Path(
        os.environ.get(
            "GITHUB_NOTIFICATIONS_LOG_DIR",
            Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local" / "state"))
            / "sketchybar",
        )
    )
    log_file = log_dir / "github-notifications-dismissed.log"
    dismiss = env_bool("GITHUB_NOTIFICATIONS_DISMISS", True)
    dry_run = env_bool("GITHUB_NOTIFICATIONS_DRY_RUN", False)

    try:
        notifications = json.load(sys.stdin)
    except json.JSONDecodeError:
        print("[]")
        return 1

    if not notifications:
        print("[]")
        return 0

    kept: list[dict] = []
    log_entries: list[dict] = []

    with ThreadPoolExecutor(max_workers=8) as pool:
        futures = [
            pool.submit(
                process_notification,
                notification,
                dismiss=dismiss,
                dry_run=dry_run,
            )
            for notification in notifications
        ]
        for future in futures:
            notification, log_entry = future.result()
            if notification is not None:
                kept.append(notification)
            if log_entry is not None:
                log_entries.append(log_entry)

    if log_entries:
        log_dir.mkdir(parents=True, exist_ok=True)
        with log_file.open("a", encoding="utf-8") as handle:
            for entry in log_entries:
                handle.write(json.dumps(entry, separators=(",", ":")) + "\n")

    json.dump(kept, sys.stdout, separators=(",", ":"))
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
