# Phase 0 spike for "run HealthSave Observatory natively on misfit".
# Same nixpkgs as this flake's lock (nixos-unstable @ 494ce7fd), the 13 runtime
# deps from nixpkgs, source from the reviewed tarball hash the module already uses.
{
  system ? builtins.currentSystem,
}:
let
  flake = builtins.getFlake "github:NixOS/nixpkgs/494ce7fd23ff6a5dff39e1fb11e9b6f2ac74bf25";
  pkgs = flake.legacyPackages.${system};
  deps =
    ps: with ps; [
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
      # for the `pip install -e .` step, run with --no-build-isolation
      setuptools
      wheel
      pip
    ];
  # nixpkgs' default python313 set is fully substituted from cache.nixos.org (87 fetches,
  # 0 builds); python312 has no substitutes for ~58 packages and some of nixpkgs'
  # own test suites fail in the sandbox (anyio 4.14.2: 5 TLS/portal tests on
  # py3.12.15). Upstream supports >=3.12,<3.14, so the spike runs 3.13.
  pythonEnv = pkgs.python313.withPackages deps;

  src = pkgs.fetchzip {
    url = "https://github.com/umutkeltek/healthsave-observatory/archive/refs/tags/v1.1.0.tar.gz";
    hash = "sha256-Lyw+Ec6tCytZPNQTHN0jWJCA7mtrdbJT0dxzPASLwqQ=";
  };
in
{
  env = pythonEnv;
  inherit src;
}
