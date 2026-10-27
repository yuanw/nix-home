{
  config,
  pkgs,
  lib,
  ...
}:

let
  inherit (lib)
    mkBefore
    mkDefault
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.services.healthsave-observatory;

  # Upstream publishes no package and its compose file does `build: .`, so the
  # release tag is the thing we pin: the Python derivation, the dashboards, the
  # migrations and the analysis config all come from v${version}, one hash.
  version = "1.1.0";
  raw = "https://raw.githubusercontent.com/umutkeltek/healthsave-observatory/v${version}";

  # Upstream's example config: every analysis job off, no LLM egress.
  defaultAnalysisConfig = pkgs.fetchurl {
    url = "${raw}/config.yaml.example";
    hash = "sha256-An0CqFEC4v7AN7m2NOU6oLcLBcdc+ZOCqh5xUbpQ7r4=";
  };

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

  # The cluster came from `initdb` inside the container image, which made
  # `healthsave` the superuser and never created a `postgres` role. Peer auth maps
  # OS user -> role, so without it postgresql-setup and any `psql` run as the
  # postgres user fail with `role "postgres" does not exist`. A separate file
  # rather than `-c`: the dollar-quoting survives neither psql's startup queries
  # nor another layer of shell quoting.
  postgresRoleSql = pkgs.writeText "healthsave-db-role.sql" ''
    DO $do$
    BEGIN
      IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'postgres') THEN
        CREATE ROLE postgres SUPERUSER LOGIN;
      END IF;
    END
    $do$;
  '';

  runDir = "/run/healthsave";
  appEnv = "${runDir}/app.env";
  grafanaEnv = "${runDir}/grafana.env";

in
{
  options.services.healthsave-observatory = {
    enable = mkEnableOption "HealthSave Observatory (TimescaleDB + FastAPI ingest API + analysis worker, native nixos services)";

    envFile = mkOption {
      type = types.path;
      example = "/run/agenix/healthsave-env";
      description = ''
        Decrypted secret with at least DB_PASSWORD and API_KEY. Read only by the
        env-rendering unit (as root); the api/worker/grafana units get the
        generated copies under ${runDir}.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "healthsave";
      description = ''
        Owns the data dir and the generated env files, and runs the api/worker
        units. The database runs as `postgres`, as everywhere else on nixos.
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

    # Loopback only, the way the container stack was: nothing is published on a
    # LAN interface, and caddy is the only door.
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
      description = "config.yaml handed to the api/worker as ANALYSIS_CONFIG (upstream ships every job off).";
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
    # disagrees with the tag this module fetches its source from, fail activation
    # rather than serve the dashboards from one release and the api/worker from
    # another.
    assertions = [
      {
        assertion = healthsavePkg.version == version;
        message = "packages/healthsave-observatory.nix is not on services.healthsave-observatory's release v${version}";
      }
    ];

    # Native, not containerised.  The containers existed only because rootless
    # podman/netavark on this nixpkgs (5.8.9) was measured broken both ways — a
    # `--network=host` client could not reach the db's loopback, and a client on a
    # private netavark bridge timed out on TCP while DNS resolved and postgres
    # accepted on its own eth0 — and running them rootful to work around that put a
    # PHI workload on root.  Native deletes the runtime and the privilege together.
    services.postgresql = {
      enable = true;

      # The extension the container image baked in, from postgresql_16's own
      # package set.  `withPackages`, not `pkgs.timescaledb` directly: the latter is
      # the bare extension and has no `psqlSchema`, which the postgresql module
      # reads (measured: "attribute 'psqlSchema' missing").
      package = pkgs.postgresql_16.withPackages (ps: [ ps.timescaledb ]);

      # In place on the 62 MB the container left behind: same PG major (16), so the
      # cluster opens where it was and the plan's dump/restore step collapsed to a
      # timescaledb version roll-forward.  The tree arrives owned by the image's
      # uid 70, which healthsave-db-adopt hands to `postgres` before the first
      # start.
      dataDir = "${cfg.dataDir}/pgdata";

      settings = {
        # The image baked this in.  Without it create_hypertable fails, and the
        # failure looks like a schema bug rather than a missing preload.
        shared_preload_libraries = "timescaledb";
        # mkDefault: the postgresql module already mkDefaults listen_addresses to
        # "localhost", and two equal-priority definitions are an eval error.
        listen_addresses = mkDefault cfg.dbBind;
        port = mkDefault cfg.dbPort;
      };
    };

    # Both loopback methods stated explicitly, because nixpkgs' default is `md5`
    # and a PG16 role stores a scram-sha-256 verifier — an md5 rule cannot
    # authenticate against that, so the api's TCP connection would die with
    # "password authentication failed".  The role was created by the container
    # entrypoint and only one of the two verifiers can match; which one is visible
    # with `select substring(rolpassword,1,14) from pg_authid`, and the loser can
    # then be deleted.  Socket stays `peer` via the nixos default, which is how the
    # units below and any administration connect.
    services.postgresql.authentication = mkBefore ''
      host    ${dbName} ${dbUser}  127.0.0.1/32  scram-sha-256
      host    ${dbName} ${dbUser}  127.0.0.1/32  md5
      host    ${dbName} ${dbUser}  ::1/128       scram-sha-256
      host    ${dbName} ${dbUser}  ::1/128       md5
    '';

    # Both already exist inside the PGDATA; these make a rebuilt dataset come up
    # with the same shape and are no-ops otherwise.
    services.postgresql.ensureUsers = [
      {
        name = dbUser;
        ensureDBOwnership = true;
      }
    ];
    services.postgresql.ensureDatabases = [ dbName ];

    # The container was killed, not shut down: the tree is 0700 and owned by the
    # image's uid 70, which the nixos `postgres` user cannot read, and its
    # postmaster.pid is days stale (postgres refuses to start if that pid has been
    # reused).  Both are one-shot fixes; the ownership test is what keeps this from
    # ever chowning behind a running server's back.
    systemd.services.healthsave-db-adopt = {
      description = "Hand the healthsave PGDATA to the postgres user";
      wantedBy = [ "multi-user.target" ];
      after = [ "healthsave-env.service" ];
      requires = [ "healthsave-env.service" ];
      before = [ "postgresql.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "healthsave-db-adopt" ''
          set -eu
          pgdata=${cfg.dataDir}/pgdata
          if [ "$(stat -c %U "$pgdata")" != postgres ]; then
            chown -R postgres:postgres "$pgdata"
          fi
          chmod 700 "$pgdata"
          rm -f "$pgdata/postmaster.pid"            # container was killed, not stopped
        '';
      };
    };

    # `initdb` never ran here, so two things the NixOS postgresql machinery takes
    # for granted are missing from this cluster: a `postgres` superuser role (see
    # postgresRoleSql), and an extension that is current in *every* database -- the
    # image's entrypoint installed timescaledb into template1, so `postgres` and
    # `template1` each carry their own version. A stale one in `postgres` is the
    # nasty one: postgresql-setup's readiness probe is `psql -d postgres` with
    # stderr sent to /dev/null, so it retries forever and activation hangs instead
    # of failing.
    #
    # The roll-forward has to be the first statement of a session, so each database
    # gets its own psql, and `-X` keeps psql's startup queries from loading the old
    # library first -- timescaledb then refuses with "cannot be updated after the
    # old version has already been loaded".
    #
    # Runs as `healthsave`, not `postgres`: this unit creates the postgres role, so
    # it cannot depend on it. Peer auth over the socket either way, so no password
    # in the store or on a command line. `ALTER EXTENSION ... UPDATE` is a no-op
    # once current, which makes it safe on every activation including a nixpkgs bump.
    systemd.services.healthsave-db-upgrade = {
      description = "Roll the timescaledb extension forward";
      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];
      # Before setup, whose `psql -d postgres` probe otherwise spins on a stale catalog.
      before = [ "postgresql-setup.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = cfg.user;
        ExecStart = pkgs.writeShellScript "healthsave-db-upgrade" ''
          set -eu
          PSQL="${config.services.postgresql.package}/bin/psql -X -q -h /run/postgresql -v ON_ERROR_STOP=1"
          $PSQL -d ${dbName} -f ${postgresRoleSql}
          for db in $($PSQL -d ${dbName} -Atc "select datname from pg_database where datname <> 'template0' order by 1"); do
            if [ -n "$($PSQL -d "$db" -Atc "select 1 from pg_extension where extname = 'timescaledb'")" ]; then
              $PSQL -d "$db" -c 'ALTER EXTENSION timescaledb UPDATE;'
            fi
          done
        '';
      };
    };

    # Dirs are created here rather than by a tmpfiles rule: activation does not
    # re-run systemd-tmpfiles-setup for a freshly added rule. DB_PASSWORD lives in
    # one place — DATABASE_URL (asyncpg, for api/worker/migrate) is derived from it
    # here instead of being pasted into the secret twice.
    systemd.services.healthsave-env = {
      description = "HealthSave Observatory directories and environment files";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "healthsave-env" ''
          set -eu
          # install -d also fixes ownership on a fresh dataset mount, which
          # arrives root:root from `zfs create`. PGDATA's own ownership is the
          # adopt unit's business, not this one's.
          #
          # 0711, not the 0750 you would give a service home: postgresql.service
          # runs User=postgres and has to *reach* ${cfg.dataDir}/pgdata, and a
          # 0750 group-only directory gives it no traverse bit -- measured as
          # pre-start dying on its first command, `rm -f .../pgdata/*.conf`,
          # with EACCES, then postgresql hitting start-limit-hit. Traverse
          # rather than membership: adding postgres to ${cfg.group} would work
          # too, but it hands postgres (and every future member) the group read
          # bit over a PHI dataset, where all it needs is to walk through. The
          # entries below stay 0700 -- pgdata belongs to postgres, the rest to
          # ${cfg.user} -- so the extra x bit leaks path names, nothing more.
          install -d -m 0711 -o ${cfg.user} -g ${cfg.group} ${cfg.dataDir}
          install -d -m 0700 ${cfg.dataDir}/pgdata
          . ${cfg.envFile}
          install -d -m 700 ${runDir}
          {
            cat ${cfg.envFile}
            printf 'DATABASE_URL=postgresql+asyncpg://%s:%s@%s:%s/%s\n' \
              '${dbUser}' "$DB_PASSWORD" '${cfg.dbBind}' '${toString cfg.dbPort}' '${dbName}'
          } > ${appEnv}
          printf 'DB_PASSWORD=%s\nHDH_GRAFANA_DATASOURCE_URL=%s:%s\n' \
            "$DB_PASSWORD" '${cfg.dbBind}' '${toString cfg.dbPort}' > ${grafanaEnv}
          chmod 600 ${appEnv} ${grafanaEnv}
          chown ${cfg.user}:${cfg.group} ${appEnv} ${grafanaEnv}
        '';
      };
    };

    # nixpkgs grafana rather than upstream's container: the only thing that
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
