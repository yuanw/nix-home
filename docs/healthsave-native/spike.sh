#!/usr/bin/env bash
# Phase 0 spike commands, run as the healthsave user (see run.sh).
set -euo pipefail
: "${HS_ENV:?}" "${HS_SRC:?}"
WORK=/tmp/hs-spike/work
export PATH="$HS_ENV/bin:$PATH"
export HOME="$WORK"
# exactly pytest's pythonpath from pyproject.toml
export PYTHONPATH="$HS_SRC:$HS_SRC/apps/api:$HS_SRC/apps/worker:$HS_SRC/apps/agents:$HS_SRC/packages/py"
export ANALYSIS_CONFIG="$WORK/config.yaml"
export HDH_PLUGINS_DIR="$HS_SRC/plugins"
set -a
# shellcheck source=/dev/null
. "$WORK/app.env"
set +a

MODULES="server worker agents analysis auth compat_v1 contracts db plugin_sdk homeassistant_mqtt runtime storage normalization replay observability"
DEPS="fastapi pydantic uvicorn sqlalchemy asyncpg litellm apscheduler prometheus-client paho-mqtt PyYAML scipy cryptography httpx"

case "${1:?usage: spike.sh <cmd>}" in
versions)
  python -c '
import importlib.metadata as m, sys
print("python", sys.version.split()[0], sys.executable)
for p in "'"$DEPS"'".split():
    try:
        print(f"{p}={m.version(p)}")
    except Exception as e:
        print(f"{p}=MISSING ({e.__class__.__name__})")
'
  ;;
imports)
  python -c '
import importlib
bad = []
for name in "'"$MODULES"'".split():
    try:
        importlib.import_module(name)
        print(f"ok   {name}")
    except Exception as e:
        bad.append(name)
        print(f"FAIL {name}: {type(e).__name__}: {e}")
raise SystemExit(1 if bad else 0)
'
  ;;
migrate)
  cd "$HS_SRC"
  python -m scripts.migrate
  ;;
api)
  cd "$HS_SRC"
  exec uvicorn server:app --host 127.0.0.1 --port 8010
  ;;
worker)
  cd "$HS_SRC"
  exec timeout 60 python -m worker.main
  ;;
worker-on)
  # apscheduler registration path: the live config ships every job off, so
  # the scheduler is never constructed. Only jobs whose next cron fire is
  # minutes away from this run, and none of these call the LLM.
  cd "$HS_SRC"
  export ANALYSIS_ANOMALY_DETECTION_ENABLED=1 ANALYSIS_RECOVERY_ENABLED=1
  exec timeout 60 python -m worker.main
  ;;
smoke)
  port="${2:-8010}" # Phase 1's pkg-api binds 8011 so both can run side by side
  cd "$HS_SRC"
  for u in /health /ready; do
    printf '%s -> %s\n' "$u" "$(curl -sS -o /tmp/hs-spike/work/out.json -w '%{http_code}' "http://127.0.0.1:$port$u") $(head -c 300 /tmp/hs-spike/work/out.json)"
  done
  printf 'no-key series -> %s\n' "$(curl -sS -o /dev/null -w '%{http_code}' "http://127.0.0.1:$port/api/v2/metrics/vital.heart_rate/series?range=30d")"
  for u in "/api/v2/metrics/vital.heart_rate/series?range=30d" "/api/v2/insights/latest"; do
    code=$(curl -sS -o /tmp/hs-spike/work/out.json -w '%{http_code}' -H "x-api-key: $API_KEY" "http://127.0.0.1:$port$u")
    printf '%s -> %s %s\n' "$u" "$code" "$(head -c 400 /tmp/hs-spike/work/out.json)"
  done
  ;;
install)
  rm -rf "$WORK/repo" "$WORK/venv"
  cp -r "$HS_SRC" "$WORK/repo"
  chmod -R u+w "$WORK/repo"
  python -m venv --system-site-packages "$WORK/venv"
  "$WORK/venv/bin/pip" install -e "$WORK/repo" --no-deps --no-build-isolation
  # from a cwd with no repo on sys.path, prove the install alone is enough
  cd /
  "$WORK/venv/bin/python" -c 'import server, worker, analysis, storage, db, plugin_sdk; print("editable install imports ok")'
  "$WORK/venv/bin/python" -c 'from storage.timescale.sync_readiness import known_contract_metrics as k; print("parity from cwd=/:", len(k()))' ||
    echo "NOTE parity.json not found with cwd=/ -> the derivation must ship it (Phase 1 postInstall)"
  ;;
parity)
  python -c '
from storage.timescale.sync_readiness import known_contract_metrics as k
from plugin_sdk.discovery import find_plugin_manifest
print("parity metrics:", len(k()))
print("whoop plugin:", find_plugin_manifest("whoop", kind="source"))
'
  ;;
install-prefix)
  # Phase 1 installs NON-editable, so the Path(__file__) ancestor walk no
  # longer reaches the repo. This is the cheapest proof of whether the
  # derivation has to ship contracts/parity.json itself.
  rm -rf "$WORK/prefix"
  "$WORK/venv/bin/pip" uninstall -y healthsave-observatory -q
  "$WORK/venv/bin/pip" install "$WORK/repo" --no-deps --no-build-isolation --prefix "$WORK/prefix" -q
  cd /
  export PP="$WORK/prefix/lib/python3.13/site-packages"
  env -u PYTHONPATH PYTHONPATH="$PP" "$WORK/venv/bin/python" -c 'import server; print("non-editable import ok")' ||
    echo "NOTE non-editable import failed"
  env -u PYTHONPATH PYTHONPATH="$PP" "$WORK/venv/bin/python" -c 'from storage.timescale.sync_readiness import known_contract_metrics as k; print("parity from cwd=/:", len(k()))' ||
    echo "NOTE non-editable: parity.json NOT found -> Phase 1 postInstall must copy it into site-packages"
  env -u PYTHONPATH PYTHONPATH="$PP" "$WORK/venv/bin/python" -c 'from plugin_sdk.discovery import find_plugin_manifest as f; print("plugin:", f("whoop", kind="source"))' ||
    echo "NOTE non-editable: plugin dir needs HDH_PLUGINS_DIR (it is set)"
  ;;
pkg-parity)
  # The two upstream packaging bugs, tested the way the units will run: the
  # *installed* tree only (no PYTHONPATH, cwd=/), so parity.json and
  # observability have to be inside $out's site-packages.
  : "${HS_PKG_ENV:?}"
  echo "pkg=$HS_PKG"
  cd /
  env -u PYTHONPATH "$HS_PKG_ENV/bin/python" -c '
import observability, server
from storage.timescale.sync_readiness import known_contract_metrics as k
from plugin_sdk.discovery import find_plugin_manifest as f
print("installed import ok (observability + server)")
print("parity metrics:", len(k()))
print("whoop plugin:", f("whoop", kind="source"))
'
  ;;
pkg-migrate)
  # scripts/ is not installed (it sits outside every `where` root), so this is
  # the reason the migrate unit gets WorkingDirectory = the source tree.
  : "${HS_PKG_ENV:?}"
  cd "$HS_SRC"
  exec env -u PYTHONPATH "$HS_PKG_ENV/bin/python" -m scripts.migrate
  ;;
pkg-api)
  # cwd=/ on purpose: `python -m` puts cwd first on sys.path, so running from
  # $HS_SRC would silently serve the *source* tree and hide any packaging gap
  # (that is how the image's flat /app hides upstream's two bugs).
  : "${HS_PKG_ENV:?}"
  cd /
  exec env -u PYTHONPATH "$HS_PKG_ENV/bin/python" -m uvicorn server:app \
    --host 127.0.0.1 --port 8011
  ;;
pkg-worker)
  : "${HS_PKG_ENV:?}"
  cd /
  exec env -u PYTHONPATH timeout 60 "$HS_PKG_ENV/bin/python" -m worker.main
  ;;
pkg-worker-on)
  : "${HS_PKG_ENV:?}"
  cd /
  export ANALYSIS_ANOMALY_DETECTION_ENABLED=1 ANALYSIS_RECOVERY_ENABLED=1
  exec env -u PYTHONPATH timeout 60 "$HS_PKG_ENV/bin/python" -m worker.main
  ;;
pkg-smoke)
  : "${HS_PKG_ENV:?}"
  "$0" smoke 8011
  ;;
*)
  echo "unknown cmd" >&2
  exit 2
  ;;
esac
