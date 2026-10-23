{
  config,
  pkgs,
  lib,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.services.healthsave-observatory;

  # Upstream publishes no package and its compose file does `build: .`, but the
  # release workflow *does* push the api/worker image to ghcr on every tag
  # (.github/workflows/release.yml). schema.sql and the analysis config are
  # fetched from the same tag so they cannot drift from the image's code.
  version = "1.1.0";
  raw = "https://raw.githubusercontent.com/umutkeltek/healthsave-observatory/v${version}";

  schemaSql = pkgs.fetchurl {
    url = "${raw}/db/schema.sql";
    hash = "sha256-sy0OcWw0uWFCdwk5CTMCB+I+fU7DiutCGKFic3ShY/w=";
  };

  # Upstream's example config: every analysis job off, no LLM egress.
  defaultAnalysisConfig = pkgs.fetchurl {
    url = "${raw}/config.yaml.example";
    hash = "sha256-An0CqFEC4v7AN7m2NOU6oLcLBcdc+ZOCqh5xUbpQ7r4=";
  };

  podman = "${pkgs.podman}/bin/podman";

  # The api/worker/migrate Python derivation (packages/healthsave-observatory.nix).
  # Instantiated here rather than taken from pkgs: colmena evaluates this node
  # against hosts/colmena.nix's meta.nixpkgs, which carries no packages/ overlay
  # (only dgx-spark has a nodeNixpkgs entry), while `nix build .#misfit` uses
  # lib/mk-pkgs.nix, which does.  callPackage gives the identical derivation on
  # either path.  Elastic-2.0 is unfree, hence the allowUnfree in both.
  healthsavePkg = pkgs.python313Packages.callPackage ../packages/healthsave-observatory.nix {
    inherit (pkgs) fetchzip;
  };

  # Upstream's grafana provisioning (6 dashboards: overview, heart, sleep,
  # activity, workouts, insights) comes from the same tag as the image. fetchzip
  # strips the archive's single top-level directory. We reuse the *package's*
  # source rather than fetching the tarball a second time, so the dashboards, the
  # plugins, db/migrations and the api/worker code can never come from different
  # releases (the assertion below ties that release to the image tag).
  observatorySrc = healthsavePkg.src;

  dbUser = "healthsave";
  dbName = "healthsave";

  runDir = "/run/healthsave";
  appEnv = "${runDir}/app.env";
  dbEnv = "${runDir}/db.env";
  grafanaEnv = "${runDir}/grafana.env";

  # The db container's first boot runs schema.sql through the postgres
  # entrypoint, whose *temporary* server accepts connections long before the
  # tables exist. Polling for the first table is what makes "migrate after db is
  # up" actually mean "after the schema is live".
  waitForSchema = pkgs.writeText "wait-for-schema.py" ''
    import asyncio
    import os
    import sys
    import time

    import asyncpg

    # asyncpg is not SQLAlchemy: it rejects the `+asyncpg` scheme that
    # DATABASE_URL carries for the app. Drop the driver suffix.
    scheme, _, rest = os.environ["DATABASE_URL"].partition("://")
    dsn = scheme.split("+")[0] + "://" + rest
    deadline = time.monotonic() + 300

    async def ready() -> bool:
        conn = await asyncpg.connect(dsn, timeout=5)
        try:
            return bool(
                await conn.fetchval(
                    "select count(*) from pg_tables where tablename = 'devices'"
                )
            )
        finally:
            await conn.close()

    async def main() -> int:
        while time.monotonic() < deadline:
            try:
                # wait_for, not a bare await: a connect that lands on the
                # entrypoint's temporary server during initdb can stall forever
                if await asyncio.wait_for(ready(), timeout=15):
                    return 0
            except (asyncio.TimeoutError, OSError, asyncpg.PostgresError):
                pass
            await asyncio.sleep(2)
        print("timed out waiting for the healthsave schema", file=sys.stderr)
        return 1

    sys.exit(asyncio.run(main()))
  '';

in
{
  options.services.healthsave-observatory = {
    enable = mkEnableOption "HealthSave Observatory (TimescaleDB + FastAPI ingest API + analysis worker, podman)";

    image = mkOption {
      type = types.str;
      default = "ghcr.io/umutkeltek/healthsave-observatory:v${version}";
      description = "api/worker/migrate image (one image, three commands).";
    };

    dbImage = mkOption {
      type = types.str;
      default = "docker.io/timescale/timescaledb:2.17.2-pg16";
      description = "Postgres image carrying the timescaledb extension.";
    };

    envFile = mkOption {
      type = types.path;
      example = "/run/agenix/healthsave-env";
      description = ''
        Decrypted secret with at least DB_PASSWORD and API_KEY. Read only by the
        env-rendering unit (as root); the containers get the generated copies
        under ${runDir}.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "healthsave";
      description = ''
        Owns the data dir and the generated env files. The containers themselves
        run rootful (see the note in the config block): rootless netavark on this
        nixpkgs drops container-to-container TCP.
      '';
    };

    group = mkOption {
      type = types.str;
      default = "healthsave";
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/healthsave";
      description = "Mountpoint of {option}`dataset`; PGDATA lives in \${dataDir}/pgdata.";
    };

    dataset = mkOption {
      type = types.str;
      default = "zroot/root/persistSave/healthsave";
      description = ''
        ZFS dataset for the data dir. Created idempotently in the initrd (this box
        rolls / back to @blank on every boot, so nothing outside a dataset
        survives) and mounted by a fileSystems entry in stage 2.
      '';
    };

    # Host networking, as upstream's compose reaches the DB too: everything is on
    # the host's loopback and nothing is published on a LAN interface. Rootless
    # `--network=host` does NOT share the host netns here (verified on the box),
    # which is why these units are rootful.
    apiBind = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Address uvicorn binds. Keep loopback; front it with caddy.";
    };

    apiPort = mkOption {
      type = types.port;
      default = 8000;
    };

    dbBind = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "postgres listen_addresses.";
    };

    dbPort = mkOption {
      type = types.port;
      default = 5432;
    };

    analysisConfigFile = mkOption {
      type = types.path;
      default = defaultAnalysisConfig;
      description = "config.yaml mounted read-only at /app/config.yaml (upstream ships every job off).";
    };

    grafana = mkEnableOption "Grafana dashboards over the observatory database (nixpkgs grafana, loopback + caddy)";

    grafanaAdminEnvFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      example = "/run/agenix/healthsave-grafana";
      description = ''
        Env file with GF_SECURITY_ADMIN_USER and GF_SECURITY_ADMIN_PASSWORD. The
        grafana module has no admin_passwordFile-style option, so this goes in as
        a systemd EnvironmentFile; nothing secret lands in the store.
      '';
    };

    grafanaPort = mkOption {
      type = types.port;
      default = 3000;
    };
  };

  config = mkIf cfg.enable {
    # The Python derivation carries its own version + tarball hash; if it ever
    # disagrees with the image tag above, fail activation rather than serve the
    # dashboards from one release and the api/worker from another.
    assertions = [
      {
        assertion = healthsavePkg.version == version;
        message = "packages/healthsave-observatory.nix is not on services.healthsave-observatory's release v${version}";
      }
    ];

    # Rootful on purpose. Upstream ships a docker-compose stack whose containers
    # are rootful too, and on this nixpkgs (podman 5.8.9 / netavark) a rootless
    # deployment was measured broken both ways: a `--network=host` client could
    # not reach the db's loopback, and a client on a private netavark bridge timed
    # out on TCP to the db container (DNS resolved, postgres accepted on its own
    # eth0, so the drop is in netavark's rootless path). Revisit rootless only
    # with a working nftables/netavark setup — then set `podman.user = cfg.user`.
    virtualisation.containers.enable = true;
    virtualisation.podman.enable = true;

    users.groups.${cfg.group} = { };
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.dataDir;
    };

    fileSystems.${cfg.dataDir} = {
      device = cfg.dataset;
      fsType = "zfs";
    };

    # Same neighbourhood as the zfs-rollback unit in hosts/misfit/configuration.nix:
    # the dataset has to exist before stage 2 mounts it.
    boot.initrd.systemd.services."zfs-create-healthsave" = {
      description = "Create ${cfg.dataset}";
      wantedBy = [ "initrd.target" ];
      requires = [ "zfs-rollback.service" ];
      after = [ "zfs-rollback.service" ];
      before = [ "sysroot.mount" ];
      unitConfig.DefaultDependencies = "no";
      serviceConfig.Type = "oneshot";
      path = [ config.boot.zfs.package ];
      script = ''
        zfs create -p -o mountpoint=legacy ${cfg.dataset} || true
      '';
    };

    # Dirs are created here rather than by a tmpfiles rule: activation does not
    # re-run systemd-tmpfiles-setup for a freshly added rule. DB_PASSWORD stays in
    # one place — DATABASE_URL (asyncpg, for api/worker/migrate) and
    # POSTGRES_PASSWORD (for the db container) are derived here instead of being
    # pasted into the secret twice.
    systemd.services.healthsave-env = {
      description = "HealthSave Observatory directories and environment files";
      wantedBy = [ "multi-user.target" ];
      before = [
        "podman-healthsave-db.service"
        "podman-healthsave-api.service"
        "podman-healthsave-worker.service"
        "healthsave-migrate.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "healthsave-env" ''
          set -eu
          # install -d also fixes ownership on a fresh dataset mount, which
          # arrives root:root from `zfs create`. PGDATA below it is chowned to the
          # postgres image's own uid by the image entrypoint on first boot.
          install -d -m 0750 -o ${cfg.user} -g ${cfg.group} ${cfg.dataDir}
          install -d -m 0700 ${cfg.dataDir}/pgdata
          . ${cfg.envFile}
          install -d -m 700 ${runDir}
          {
            cat ${cfg.envFile}
            printf 'DATABASE_URL=postgresql+asyncpg://%s:%s@%s:%s/%s\n' \
              '${dbUser}' "$DB_PASSWORD" '${cfg.dbBind}' '${toString cfg.dbPort}' '${dbName}'
          } > ${appEnv}
          printf 'POSTGRES_DB=%s\nPOSTGRES_USER=%s\nPOSTGRES_PASSWORD=%s\n' \
            '${dbName}' '${dbUser}' "$DB_PASSWORD" > ${dbEnv}
          printf 'DB_PASSWORD=%s\nHDH_GRAFANA_DATASOURCE_URL=%s:%s\n' \
            "$DB_PASSWORD" '${cfg.dbBind}' '${toString cfg.dbPort}' > ${grafanaEnv}
          chmod 600 ${appEnv} ${dbEnv} ${grafanaEnv}
          chown ${cfg.user}:${cfg.group} ${appEnv} ${dbEnv} ${grafanaEnv}
        '';
      };
    };

    virtualisation.oci-containers.containers = {
      healthsave-db = {
        image = cfg.dbImage;
        networks = [ "host" ];
        environmentFiles = [ dbEnv ];
        volumes = [
          "${cfg.dataDir}/pgdata:/var/lib/postgresql/data"
          "${schemaSql}:/docker-entrypoint-initdb.d/001_schema.sql:ro"
        ];
        # trailing args go to postgres, not to the entrypoint's initdb path
        cmd = [
          "postgres"
          "-c"
          "listen_addresses=${cfg.dbBind}"
        ];
      };

      healthsave-api = {
        image = cfg.image;
        networks = [ "host" ];
        environmentFiles = [ appEnv ];
        volumes = [ "${cfg.analysisConfigFile}:/app/config.yaml:ro" ];
        cmd = [
          "uvicorn"
          "server:app"
          "--host"
          cfg.apiBind
          "--port"
          (toString cfg.apiPort)
        ];
      };

      healthsave-worker = {
        image = cfg.image;
        networks = [ "host" ];
        environmentFiles = [ appEnv ];
        volumes = [ "${cfg.analysisConfigFile}:/app/config.yaml:ro" ];
        cmd = [
          "python"
          "-m"
          "worker.main"
        ];
      };
    };

    # oci-containers has no job type (it always runs `-d --sdnotify`), so the
    # one-shot migration is its own unit.
    systemd.services.healthsave-migrate = {
      description = "HealthSave Observatory schema migrations";
      wantedBy = [ "multi-user.target" ];
      after = [ "healthsave-env.service" ];
      requires = [ "healthsave-env.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        TimeoutStartSec = "10min";
        ExecStart = pkgs.writeShellScript "healthsave-migrate" ''
          set -eu
          ${podman} run --rm --network host \
            --env-file ${appEnv} \
            -v ${waitForSchema}:/wait-for-schema.py:ro \
            ${cfg.image} python /wait-for-schema.py
          exec ${podman} run --rm --network host \
            --env-file ${appEnv} \
            ${cfg.image} python -m scripts.migrate
        '';
      };
      # a first boot pulls ~500 MB, and a pull hiccup must not trip the default
      # start limit and wedge the stack until a manual reset-failed
      unitConfig.StartLimitIntervalSec = 0;
    };

    systemd.services.podman-healthsave-db = {
      after = [ "healthsave-env.service" ];
      requires = [ "healthsave-env.service" ];
      unitConfig.StartLimitIntervalSec = 0;
    };

    # migrations before anything reads or writes the schema
    systemd.services.podman-healthsave-api = {
      after = [ "healthsave-migrate.service" ];
      requires = [ "healthsave-migrate.service" ];
      unitConfig.StartLimitIntervalSec = 0;
    };

    systemd.services.podman-healthsave-worker = {
      after = [ "healthsave-migrate.service" ];
      requires = [ "healthsave-migrate.service" ];
      unitConfig.StartLimitIntervalSec = 0;
    };

    # nixpkgs grafana rather than upstream's container: the only thing that
    # surface needs is a postgres connection, and the repo's dashboards are plain
    # JSON files that the module can provision straight off the store.
    services.grafana = mkIf cfg.grafana {
      enable = true;
      settings = {
        server = {
          http_addr = "127.0.0.1";
          http_port = cfg.grafanaPort;
          domain = "grafana.yuanw.me";
          root_url = "https://%(domain)s/";
        };
        analytics = {
          reporting_enabled = false;
          check_for_updates = false;
          check_for_plugin_updates = false;
        };
      };
      provision.datasources.settings.datasources = [
        {
          name = "HealthSave";
          uid = "healthsave-pg";
          type = "postgres";
          # expanded by grafana from the unit's EnvironmentFiles
          url = "\${HDH_GRAFANA_DATASOURCE_URL}";
          user = dbUser;
          secureJsonData.password = "\${DB_PASSWORD}";
          jsonData = {
            database = dbName;
            sslmode = "disable";
            timescaledb = true;
            postgresVersion = 1600;
          };
          isDefault = true;
          editable = true;
        }
      ];
      provision.dashboards.path = "${observatorySrc}/deploy/grafana/dashboards";
    };

    systemd.services.grafana = mkIf cfg.grafana {
      after = [ "healthsave-env.service" ];
      requires = [ "healthsave-env.service" ];
      # grafana reads these before dropping to its own user, so they can stay
      # root-owned 0600
      serviceConfig.EnvironmentFiles = [
        cfg.grafanaAdminEnvFile
        grafanaEnv
      ];
    };
  };
}
