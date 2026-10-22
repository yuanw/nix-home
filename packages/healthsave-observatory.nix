# The Python backend (api + worker + migrate) of HealthSave Observatory, built
# from upstream's release tarball instead of ghcr.io's rootful image.  Phase 1 of
# ~/org/denote/20261021T181404--run-healthsave-observatory-natively-on-misfit__health_misfit_nixos_healthsave_plan.org
# — modules/healthsave-observatory.nix still runs the containers until Phase 5.
#
# Three things here work around upstream packaging, not nixpkgs: the `include`
# omission, the uninstalled contracts/parity.json and the uninstalled plugins/
# tree.  All three are invisible in the image, which flattens the repo into /app
# with explicit COPY lines.
#
# python313, not 312: upstream allows >=3.12,<3.14, and on this pin python312
# has no cache.nixos.org substitutes for ~58 deps (some of its own suites also
# fail in the sandbox) while python313 fetches all 87 paths and builds nothing.
# The interpreter arrives as `python` from the python-specific callPackage
# (python313Packages.callPackage), so a future nixpkgs default cannot silently
# move us to an unsupported 3.14.
#
# `version` is the one release this tree must match: modules/healthsave-observatory.nix
# builds its dashboards/migrations/plugins source from this derivation's `src` and
# asserts this `version` against the container image's tag, so the api/worker and
# the Grafana dashboards cannot come from different releases.
{
  lib,
  fetchzip,
  buildPythonPackage,
  python,
  setuptools,
  wheel,
  fastapi,
  pydantic,
  uvicorn,
  sqlalchemy,
  asyncpg,
  litellm,
  apscheduler,
  prometheus-client,
  paho-mqtt,
  pyyaml,
  scipy,
  cryptography,
  httpx,
  # Upstream asks for uvicorn[standard].  nixpkgs' uvicorn propagates only
  # click+h11 and there is no uvicorn-uvloop attribute, so without these five we
  # silently lose the event-loop/HTTP accelerators Phase 0 actually measured
  # (it ran `pip install -e .`, which pulled them).
  uvloop,
  httptools,
  setproctitle,
  python-dotenv,
  watchfiles,
  pytestCheckHook,
  # to relax upstream's five exact pins (see pythonRelaxDeps below)
  pythonRelaxDepsHook,
}:
# buildPythonPackage, not ...Application: toPythonApplication sets
# passthru.pythonModule = false, and `python313.withPackages` (the env the units
# start) only collects packages whose pythonModule matches the interpreter — an
# application was measured to land in an env with an empty site-packages.  We run
# the app as `python -m uvicorn server:app` / `python -m worker.main` anyway, so
# there is no console script to own.
buildPythonPackage rec {
  pname = "healthsave-observatory";
  version = "1.1.0";
  pyproject = true;

  src = fetchzip {
    url = "https://github.com/umutkeltek/healthsave-observatory/archive/refs/tags/v${version}.tar.gz";
    hash = "sha256-Lyw+Ec6tCytZPNQTHN0jWJCA7mtrdbJT0dxzPASLwqQ=";
  };

  build-system = [
    setuptools
    wheel
  ];

  dependencies = [
    fastapi
    pydantic
    uvicorn
    sqlalchemy
    asyncpg
    litellm
    apscheduler
    prometheus-client
    paho-mqtt
    pyyaml
    scipy
    cryptography
    httpx
    uvloop
    httptools
    setproctitle
    python-dotenv
    watchfiles
  ];

  nativeBuildInputs = [
    pytestCheckHook
    pythonRelaxDepsHook
  ];

  # Upstream pins five deps exactly (`fastapi==0.115.0` …) and nixpkgs is ahead of
  # every one of them, which pythonRuntimeDepsCheck refuses.  Phase 0 measured
  # nixpkgs' versions serving real data on the live database, so the pins are
  # relaxed rather than honoured with fetchPypi — if a future release breaks on
  # that, the fix is fetchPypi + dependency_version_override for these five only.
  pythonRelaxDeps = [
    "fastapi"
    "pydantic"
    "uvicorn"
    "sqlalchemy"
    "asyncpg"
  ];

  # [tool.setuptools.packages.find] include lists 14 names but packages/py/ holds
  # 13 dirs: `observability*` is the one that never ships, so a non-editable
  # install dies on `import server` -> ModuleNotFoundError: observability.
  # Pure .py (2 files, no data), so editing the include list beats a patch file.
  # If upstream ever reorders the list this fails loudly at build time, which is
  # the right failure mode until the bug is fixed upstream and this can die.
  postPatch = ''
    substituteInPlace pyproject.toml \
      --replace-fail '"plugin_sdk*",' '"plugin_sdk*", "observability*",'
  '';

  # Two things sit outside every `where` root and so never get installed:
  #   - contracts/parity.json, which the runtime looks up as
  #     <site-packages>/contracts/parity.json from a site-packages layout, and
  #   - the whole plugins/ tree, which the app imports as real modules
  #     (server/api/v2_sources.py:38 `from plugins.sources.whoop import
  #     WhoopSource`, agents/main.py:59 the anomaly feed).  It is a PEP 420
  #     namespace tree (no plugins/__init__.py), so setuptools `find` cannot
  #     reach it even from a `.` root -- upstream's Dockerfile flattens it into
  #     /app with a COPY.  252K, so copy it the same way and keep the plugin.yaml
  #     manifests alongside.  HDH_PLUGINS_DIR still wins for discovery.
  postInstall = ''
    mkdir -p "$out/${python.sitePackages}/contracts"
    cp "$src/contracts/parity.json" "$out/${python.sitePackages}/contracts/"
    cp -r "$src/plugins" "$out/${python.sitePackages}/plugins"
  '';

  # The full suite needs pytest-asyncio and a live database.  This subset needs
  # neither and covers the one file postInstall hand-copies.  enabledTestPaths
  # (not the removed pytestFlagsArray) aborts the build if upstream renames it.
  doCheck = true;
  enabledTestPaths = [ "tests/contract/test_parity_manifest_valid.py" ];

  # Every top-level package, so the next `include` omission cannot pass silently.
  # `plugins` is in here because it is the one the derivation ships by hand.
  # litellm and its client are here because upstream imports litellm *inside a
  # method* (tests/test_llm_client.py greps the source to forbid a top-level
  # import), so no other build-time check ever touches it.
  # `scripts` is deliberately absent — it sits at the repo root, outside every
  # `where` root, so it is never installed; `python -m scripts.migrate` only
  # works with WorkingDirectory = the source tree.
  pythonImportsCheck = [
    "server"
    "plugins.sources.whoop"
    "worker"
    "agents"
    "analysis"
    "auth"
    "compat_v1"
    "contracts"
    "db"
    "runtime"
    "homeassistant_mqtt"
    "storage"
    "normalization"
    "replay"
    "plugin_sdk"
    "observability"
    "litellm"
    "analysis.llm.client"
  ];

  meta = {
    description = "Self-hosted health-data backend: FastAPI ingest API, analysis worker and TimescaleDB schema";
    homepage = "https://github.com/umutkeltek/healthsave-observatory";
    license = lib.licenses.elastic20;
  };
}
