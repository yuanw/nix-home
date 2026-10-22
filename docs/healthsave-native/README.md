# HealthSave Observatory — Phase 0 native spike (misfit, 2026-10-21)

Spike for [[file:~/org/denote/20261021T181404--run-healthsave-observatory-natively-on-misfit__health_misfit_nixos_healthsave_plan.org]]:
does the upstream app run natively on nixpkgs' python packages, against the *live*
container database, before any systemd unit is written.

**Verdict: go.** Both processes served real PHI-backed data on nixpkgs'
fastapi 0.141 / pydantic 2.13 / uvicorn 0.51. No pydantic/fastapi breakage.
Two upstream packaging bugs to work around in Phase 1 (below).

## Run it

Copy the three files to the box and run subcommands as root (they read
`/run/healthsave/app.env` then drop to the `healthsave` user):

    scp spike.nix run.sh spike.sh misfit:/tmp/hs-spike/
    ssh misfit 'sudo /tmp/hs-spike/run.sh versions'    # imports, migrate, parity,
                                                       # api, smoke, worker,
                                                       # worker-on, install, install-prefix
    ssh misfit 'sudo /tmp/hs-spike/run.sh api &'       # uvicorn on 127.0.0.1:8010
    ssh misfit 'sudo /tmp/hs-spike/run.sh smoke'
    ssh misfit 'sudo pkill -u healthsave -f uvicorn'   # never pkill -f "port 8010"
                                                       # from the ssh session: it matches
                                                       # the ssh command line itself

`spike.nix` pins the same nixpkgs as `flake.lock` (nixos-unstable @ `494ce7fd`)
and the same reviewed tarball hash the module already uses for Grafana.

## Environment

`run.sh` builds `spike.nix -A env` and runs with
`PYTHONPATH = <src>:<src>/apps/api:<src>/apps/worker:<src>/apps/agents:<src>/packages/py`
(exactly pytest's `pythonpath` in `pyproject.toml`), `cwd=<src>`,
`ANALYSIS_CONFIG=<the container's live config.yaml>`, `HDH_PLUGINS_DIR=<src>/plugins`.

| dep | nixpkgs (used) | container image |
|-----|----------------|-----------------|
| python | 3.13.15 | 3.12.15 |
| fastapi | 0.141.1 | 0.115.0 |
| pydantic | 2.13.4 | 2.9.2 |
| uvicorn | 0.51.0 | 0.30.0 |
| sqlalchemy | 2.0.51 | 2.0.35 |
| asyncpg | 0.31.0 | 0.30.0 |
| litellm | 1.102.1 | 1.83.0 |
| apscheduler | 3.11.2 | 3.11.3 |
| prometheus-client | 0.25.0 | |
| paho-mqtt | 2.1.0 | |
| PyYAML | 6.0.3 | 6.0.3 |
| scipy | 1.18.1 | 1.18.1 |
| cryptography | 50.0.0 | 50.0.2 |
| httpx | 0.28.1 | 0.28.1 |

The five version drifts the plan predicted turned out to be inert.
`constraints.txt` is `podman exec healthsave-api pip freeze` — the 68-package
transitive set the image actually runs, kept for Phase 1 pinning if needed.

## Results

| check | result |
|-------|--------|
| all 15 top-level modules import | ok (server worker agents analysis auth compat_v1 contracts db plugin_sdk homeassistant_mqtt runtime storage normalization replay observability) |
| `python -m scripts.migrate` | `schema already up-to-date (27 tracked)` against the live db |
| uvicorn boot | clean; Apple Health plugin loaded, `storage backend resolved: PostgresIngestStorage` |
| `/health` `/ready` | 200 `{"status":"ok"}` / `{"status":"ok","database":"ok"}` |
| series without key | 401 |
| series with key | 200, real 30 d heart-rate points |
| `/api/v2/insights/latest` | 200 (null briefings — jobs are off in config.yaml) |
| `python -m worker.main` 60 s | clean start/stop, no ImportError |
| worker with jobs enabled | apscheduler 3.11 registers `anomaly_check` + `recovery_check`, scheduler starts and shuts down cleanly |
| `pip install -e .` (setuptools, multi-root `where`) | builds, installs, imports from `/` |

## Phase 1 blockers (both are upstream packaging bugs)

1. **`observability` is not installed.** `[tool.setuptools.packages.find] include`
   in `pyproject.toml` omits `observability*`, so a non-editable install has no
   `observability` module and even `import server` dies with
   `ModuleNotFoundError: No module named 'observability'`. The Dockerfile hides
   this by `COPY packages/py/observability/ ./observability/`. Phase 1 must ship
   it (copy in `postInstall`, or add it to the `include` list with a patch).
   `pythonImportsCheck = [ "server" ... ]` catches it, so keep that check.
2. **`contracts/parity.json` is not installed.** With a site-packages layout the
   `Path(__file__)` walk searches `<site-packages>/contracts/parity.json`, so the
   plan's `postInstall` copy into `$out/${sitePackages}/contracts/` is required,
   not optional. (Under an *editable* install it is found via the repo layout.)
3. Plugin discovery: `HDH_PLUGINS_DIR=<src>/plugins` is sufficient — it wins over
   the ancestor walk, verified from a site-packages layout.

## python 3.13, not 3.12

Upstream allows `>=3.12,<3.14`. On this pin of nixos-unstable,
`python312Packages` has **no cache.nixos.org substitutes for ~58 packages** and
some of nixpkgs' own suites fail in the sandbox (`anyio 4.14.2`: 5 TLS/portal
tests on py3.12.15), which is a rebuild tax on every deploy and a hard blocker
without a `dontUsePytestCheck` override. `python313Packages` fetches all 87
paths and builds nothing. Phase 1 should use `python313`.

## Notes

- `/etc/healthsave/config.yaml` does not exist yet — the module mounts the store copy of
  `config.yaml.example` at `/app/config.yaml`. The spike used `podman cp` of the container's live
  `/app/config.yaml` (every analysis job off, no LLM egress) as `ANALYSIS_CONFIG`, which is the
  same bytes Phase 2/3 should put at `/etc/healthsave/config.yaml`.
- `ANALYSIS_CONFIG` is read by `apps/api/server/main.py:126`,
  `apps/worker/worker/main.py:40` and `apps/agents/agents/main.py:83` — one env
  var covers api + worker + agents.
- cwd can be the read-only store path: nothing wrote to it during the spike.
- Nothing on the live stack was changed: the container units stayed `active`,
  the container api still answered `/health` 200 afterwards, and the spike bound
  8010 only.
