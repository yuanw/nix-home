#!/usr/bin/env bash
# Shared preview for openwrt-diff and openwrt-apply: the declared batch, run against a
# throwaway copy of /etc/config under /tmp, plus the notes that make the diff safe to
# read. Batch on stdin, router in $host.
#
# Nothing in here commits to the router. libuci writes its delta to /tmp/.uci no matter
# what `-c` says, and the old cleanup was `uci revert` over every config -- which
# quietly discarded any staged edit that was not ours, from the recipe whose whole
# contract is "nothing on the router modified". So park the delta and put it back
# byte-for-byte instead.
#
# Both sides of every comparison are sorted, because wipe-and-re-add moves the sections
# it touches to the end of the file. What sorting cannot see is *order*, and for
# firewall rules order is precedence, so the rule sequence is checked separately.
set -eu -o pipefail
host=${host:-root@192.168.1.1}
batch=$(cat)

# Anonymous types the batch re-creates, as `config.type`, taken from the batch's own
# `add` lines so nothing here depends on how many sections a router happens to have.
wipes=$(printf '%s\n' "$batch" | awk '$1 == "add" { print $2 "." $3 }' | sort -u | tr '\n' ' ')

before=$(ssh -n "$host" 'uci show')
after=$(printf '%s\n' "$batch" | ssh "$host" "
    set -eu
    park=/tmp/.uci.nix-uci-park
    rm -rf \$park; cp -a /tmp/.uci \$park 2>/dev/null || mkdir -p \$park
    rm -rf /tmp/.uci; mkdir -p /tmp/.uci
    rm -rf /tmp/nix-uci-diff; mkdir -p /tmp/nix-uci-diff
    cp /etc/config/* /tmp/nix-uci-diff/
    for t in $wipes; do
        while uci -q -c /tmp/nix-uci-diff delete \"\${t%%.*}.@\${t#*.}[0]\"; do :; done
    done
    # batch prints the generated name of every add section: not config content.
    uci -c /tmp/nix-uci-diff batch > /dev/null
    for f in /tmp/nix-uci-diff/*; do
        uci -c /tmp/nix-uci-diff commit \"\$(basename \"\$f\")\" 2>/dev/null || true
    done
    uci -c /tmp/nix-uci-diff show
    rm -rf /tmp/nix-uci-diff
    rm -rf /tmp/.uci; mv \$park /tmp/.uci
    true")

if diff -u <(printf '%s\n' "$before" | sort) <(printf '%s\n' "$after" | sort); then
    echo 'no changes: the router already matches the declared config'
fi

# Which services need reloading. The signs must be +/-, so the unified diff above is
# the only correct source: a plain diff marks changes with < and >, and that is how
# applies came to commit config for weeks without reloading anything. Hyphens have to
# be allowed or `https-dns-proxy` disappears from this list.
printf 'CHANGED: %s\n' "$(diff -u <(printf '%s\n' "$before" | sort) <(printf '%s\n' "$after" | sort) \
    | sed -nE 's/^[-+]([a-z][a-z0-9_-]*)\..*/\1/p' | sort -u | tr '\n' ' ')"

if ! diff -q <(printf '%s\n' "$before" | grep -E '^firewall\.@rule\[[0-9]+\]\.name=' | sed "s/.*=//;s/'$//") \
    <(printf '%s\n' "$after" | grep -E '^firewall\.@rule\[[0-9]+\]\.name=' | sed "s/.*=//;s/'$//") >/dev/null 2>&1; then
    echo 'NOTE: firewall rule order differs from the declaration; first match wins.'
fi

# An anonymous type is wiped and re-added, so a section that is live but not declared
# here is deleted, not left alone -- the LuCI port forward or the hand-written static
# lease. Say it before the confirmation prompt, because that is the one the diff
# invites you to skim past.
for t in $wipes; do
    live=$(printf '%s\n' "$before" | grep -cE "^${t%%.*}\.@${t#*.}\[[0-9]+\]=" || true)
    added=$(printf '%s\n' "$batch" | awk -v c="${t%%.*}" -v t="${t#*.}" '$1 == "add" && $2 == c && $3 == t { n++ } END { print n + 0 }')
    if [ "${live:-0}" -gt "$added" ]; then
        echo "WARNING: ${t} has ${live} live sections and the declaration re-adds ${added}; the apply would delete $((live - added)) of them"
    fi
done
