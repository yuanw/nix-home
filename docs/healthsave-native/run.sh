#!/usr/bin/env bash
# Phase 0 spike runner, run as root on misfit:
#   sudo /tmp/hs-spike/run.sh <cmd>
# Loads the live container's env + config, then drops to the healthsave user.
set -euo pipefail
DIR=/tmp/hs-spike
CMD=${1:?usage: run.sh versions|imports|migrate|api|worker|smoke|install}
cd "$DIR"
chmod 0755 "$DIR"

if [ ! -f paths.env ]; then
  env=$(nix-build --impure -Q ./spike.nix -A env --no-out-link)
  src=$(nix-build --impure -Q ./spike.nix -A src --no-out-link)
  printf 'HS_ENV=%s\nHS_SRC=%s\n' "$env" "$src" >paths.env
fi
# shellcheck source=/dev/null
. ./paths.env

install -d -m 0777 "$DIR/work"
install -m 0600 -o healthsave -g healthsave /run/healthsave/app.env "$DIR/work/app.env"
if [ ! -f "$DIR/work/config.yaml" ]; then
  podman cp healthsave-api:/app/config.yaml "$DIR/work/config.yaml"
  chown healthsave:healthsave "$DIR/work/config.yaml"
  chmod 0600 "$DIR/work/config.yaml"
fi

exec setpriv --reuid=healthsave --regid=healthsave --init-groups \
  env HOME="$DIR/work" HS_ENV="$HS_ENV" HS_SRC="$HS_SRC" "$DIR/spike.sh" "$CMD"
