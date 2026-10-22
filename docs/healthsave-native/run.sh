#!/usr/bin/env bash
# Phase 0 spike runner, run as root on misfit:
#   sudo /tmp/hs-spike/run.sh <cmd>
# Loads the live container's env + config, then drops to the healthsave user.
set -euo pipefail
DIR=/tmp/hs-spike
CMD=${1:?usage: run.sh versions|imports|migrate|api|worker|smoke|install|pkg-*}
cd "$DIR"
chmod 0755 "$DIR"

# Phase 1: the derivation itself.  `nix-build -A pkg` is the gate — it runs
# pythonImportsCheck and the contract subset inside the sandbox.
if [ "$CMD" = pkg-build ]; then
  exec env NIXPKGS_ALLOW_UNFREE=1 nix-build --impure -Q ./spike.nix -A pkg --no-out-link
fi

if [ ! -f paths.env ]; then
  env=$(nix-build --impure -Q ./spike.nix -A env --no-out-link)
  src=$(nix-build --impure -Q ./spike.nix -A src --no-out-link)
  printf 'HS_ENV=%s\nHS_SRC=%s\n' "$env" "$src" >paths.env
fi
# shellcheck source=/dev/null
. ./paths.env

# The runtime env the Phase 3 units will use: the installed package and its
# propagated deps, nothing else on sys.path.
case "$CMD" in
pkg-*)
  if [ ! -f pkgpaths.env ]; then
    pkg=$(NIXPKGS_ALLOW_UNFREE=1 nix-build --impure -Q ./spike.nix -A pkg --no-out-link)
    pkg_env=$(NIXPKGS_ALLOW_UNFREE=1 nix-build --impure -Q ./spike.nix -A pkgEnv --no-out-link)
    printf 'HS_PKG=%s\nHS_PKG_ENV=%s\n' "$pkg" "$pkg_env" >pkgpaths.env
  fi
  # shellcheck source=/dev/null
  . ./pkgpaths.env
  ;;
esac

install -d -m 0777 "$DIR/work"
install -m 0600 -o healthsave -g healthsave /run/healthsave/app.env "$DIR/work/app.env"
if [ ! -f "$DIR/work/config.yaml" ]; then
  podman cp healthsave-api:/app/config.yaml "$DIR/work/config.yaml"
  chown healthsave:healthsave "$DIR/work/config.yaml"
  chmod 0600 "$DIR/work/config.yaml"
fi

exec setpriv --reuid=healthsave --regid=healthsave --init-groups \
  env HOME="$DIR/work" HS_ENV="$HS_ENV" HS_SRC="$HS_SRC" \
  HS_PKG="${HS_PKG:-}" HS_PKG_ENV="${HS_PKG_ENV:-}" "$DIR/spike.sh" "$CMD"
