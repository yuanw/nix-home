#!/bin/sh
# Publish the GitHub inbox to OmniWM's shell vars so the bar widget in
# modules/wm/omniwm/widgets/github can render it. This is what
# modules/wm/sketchybar/plugins/github.sh did for sketchybar: gh does the
# auth, github-notifications-filter.py drops closed pull requests and team
# mentions (and logs them), and the result goes to the bar. Nothing here needs
# a token of its own, and the widget never touches the network.
#
# launchd runs it on a timer (see modules/wm/omniwm.nix) because a widget
# cannot start a program; sketchybar ran the plugin on mouse.entered instead.
#
# Required environment: FILTER (the python filter), and gh/python3/jq/omniwmctl
# on PATH -- the launchd agent sets both.
set -eu
# ${#json} must count bytes, and the value bound below is a byte bound
LC_ALL=C

KEY=github-inbox
# omniwmctl drops a value over 4096 bytes (IPCShellRequest.valueLimit); rows
# are trimmed until the JSON fits, and "n" keeps the real count for the bell.
MAX_BYTES=3900
ROWS=12
TITLE=120

fail() {
	exec omniwmctl shell set "$KEY" '{"n":0,"i":[],"e":"gh api notifications failed"}'
}

if ! threads=$(python3 "$FILTER" 2>/dev/null) || ! printf '%s' "$threads" | jq -e 'type == "array"' >/dev/null 2>&1; then
	fail
fi

json=$(printf '%s' "$threads" | jq -c --argjson rows "$ROWS" --argjson title "$TITLE" '
    {n: length} + {i: [.[:$rows][] | {
        r: (.repository.full_name // .repository.name // ""),
        h: (.repository.html_url // ""),
        t: ((.subject.title // "")[0:$title]),
        k: (.subject.type // ""),
        s: (.subject.url // ""),
        p: (.subject.html_url // "")
    }]}')

count=$(printf '%s' "$json" | jq '.i | length')
while [ "$count" -gt 1 ] && [ "${#json}" -gt "$MAX_BYTES" ]; do
	json=$(printf '%s' "$json" | jq -c '.i |= .[:-1]')
	count=$((count - 1))
done

exec omniwmctl shell set "$KEY" "$json"
