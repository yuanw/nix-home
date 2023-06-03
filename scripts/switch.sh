#!/usr/bin/env sh

set -euo pipefail

export NIXPKGS_ALLOW_UNFREE=1

nixos-rebuild switch --use-remote-sudo --flake '.#bren' --impure

doom sync
