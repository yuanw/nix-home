# How nixos-config Does Logging and Monitoring with `modules/services/observability`

Findings from reading `../nixos-config`: `modules/services/observability/{loki,prometheus,grafana,ntfy,gatus,beszel,dockhand,monit,cockpit,myspeed,netalertx,portainer,scanopy,termix}`, `hosts/*.nix`, `lib/consts.nix`, `lib/helpers.nix`, `secrets/observability/`.

## TL;DR

- Two aggregation nodes: **vm-monitor** (Prometheus + Alertmanager + Grafana + Loki server, plus container UIs) and **cloud-observe** (AWS box running **ntfy** and **Gatus** as the external alert/uptime edge).
- **Logging**: journald on every host → **Grafana Alloy** (`loki.source.journal`) → push to a single **Loki** server on vm-monitor → queried from Grafana via a provisioned Loki datasource + a custom "Systemd Logs Viewer" dashboard.
- **Metrics**: per-host Prometheus exporters (node, nginx, podman, restic, smartctl, wireguard, libvirt, crowdsec) → scraped by the Prometheus server on vm-monitor. Targets are **generated at eval time** by inspecting every host's `nixosConfigurations` config — no runtime service discovery, no hand-written scrape lists.
- **Alerting** all lands in **ntfy**: Prometheus rules → Alertmanager → `alertmanager-ntfy` publisher → topic `prometheus-alerts`; Gatus → topic `gatus-alerts`. Whether ntfy exists anywhere is itself detected across all hosts (`anyHostEnabled`), and everything else degrades gracefully without it.
- Ports, subdomains, UIDs, ntfy topics live in `lib/consts.nix`; which services run where is exposed through the `getEnabledServices` registry in `lib/helpers.nix`, which Gatus uses to auto-build its endpoint list.
- Telemetry data (Loki chunks, Prometheus TSDB, Grafana plugins/logs) is explicitly **excluded from restic backups**.

## Who runs what

| Host | Observability role |
|---|---|
| `vm-monitor` (infra VM on `hypervisor`) | Prometheus server + Alertmanager + alertmanager-ntfy, Grafana, Loki **server**, Beszel **hub**, Dockhand **server**, MySpeed, Termix; own exporters (nginx, node, podman, restic) + Loki agent |
| `cloud-observe` (AWS, WireGuard + cloudflared) | **ntfy** server, **Gatus**, Loki agent, Beszel agent, nginx+node exporters (exporters bound to `wg0`, `scrapeAddress` = WG address) |
| `hypervisor` | Loki agent, Beszel agent (bound to VLAN iface), **Cockpit**, libvirt + smartctl + node + nginx exporters |
| `vm-network` | Loki agent, Beszel agent, **NetAlertX**, nginx/node/podman/restic/**wireguard** exporters (bound to `infraInterface`) |
| `pi`, `vm-public`, `vm-app` | Loki agent, Beszel agent, **Dockhand agent**, nginx/node/podman(+restic) exporters |
| `desktop`, `framework` | smartctl exporter only |
| nowhere (module exists, no host enables it) | `monit`, `portainer`, `scanopy`, crowdsec exporter |

All of this is just flags in each `hosts/<name>.nix` under `custom.services.observability.*`, e.g. `hosts/vm-monitor.nix`:

```nix
observability = {
  grafana.enable = true;
  loki = { server.enable = true; agent.enable = true; };
  prometheus = { server.enable = true; exporters.node.enable = true; };
  ...
};
```

## Log pipeline: journald → Alloy → Loki → Grafana

**Server** (`loki/server.nix`): single-node Loki, `auth_enabled = false`, in-memory ring, replication factor 1, TSDB schema on the filesystem (`/var/lib/loki/chunks`), port `ports.loki.server` (3100), firewall opened, analytics off. Data dir added to `custom.services.infra.restic.extraExcludes` — logs are hot data, not backed up.

**Agent** (`loki/agent.nix`): each host runs `services.alloy` with a generated config in `/etc/alloy/loki-journal.alloy` that:

- tails the **journal** (`loki.source.journal`, `max_age = 12h`),
- relabels: `__journal__systemd_unit` → `unit` label, adds `job="systemd-journal"` and `host=<hostName>`,
- pushes to `http://<serverAddress>:3100/loki/api/v1/push`.

`serverAddress` **defaults to `helpers.getHostAddress "vm-monitor"`**, so enabling the agent anywhere ships its journal home with zero config (pi overrides it explicitly; cloud-observe reaches vm-monitor over WireGuard). Journal itself is kept persistent, capped at 1 GiB by `roles.headless.services` (256 MiB on cloud platforms), and `logrotate` is on.

**Read path**: Grafana provisions a Loki datasource with `uid = "loki"` and ships a checked-in dashboard, `grafana/systemd-logs.json`, with `host`/`unit` template variables driven by `label_values({job="systemd-journal"}, ...)`.

Note: journald → Alloy is the *only* log path here; journald does not forward to Loki directly, and Wazuh (a separate `security` namespace, not observability) is the SIEM doing its own journald collection.

## Metrics pipeline: exporters → Prometheus

**Exporters** (`prometheus/exporters.nix`) — one option per exporter under `custom.services.observability.prometheus.exporters.<name>.enable`, all ports from `consts.ports.prometheus.exporters`:

- `node` with extra collectors `systemd`, `processes`, `tcpstat`;
- `nginx` — scrapes a localhost-only `stub_status` server that the **nginx module injects into nginx.conf** when the exporter is enabled (`modules/services/networking/nginx/default.nix`, port `ports.nginx.stub` 8082);
- `restic` — reuses `repository`/`passwordFile` straight from the `data-local` restic backup profile, so backup health is monitored with zero duplicated config;
- `podman` — an OCI sidecar (`quay.io/navidys/prometheus-podman-exporter`) mounting `/run/podman/podman.sock` read-only;
- `crowdsec` — not an exporter binary, just `crowdsec.settings.general.prometheus` enabled;
- `libvirt` (read-only socket `libvirt-sock-ro`), `smartctl`, `wireguard`.

Two structural ideas:

1. **Assertions gate each exporter on its prerequisite module** — `libvirt` requires `infra.hypervisor.enable`, `nginx` requires `networking.nginx.enable`, `restic` requires `infra.restic.enable`, etc. You cannot misconfigure a host into scraping a thing that isn't there.
2. **Networking is declarative per host**: `exporters.interface` restricts firewall openings to one interface (used by vm-network, hypervisor, cloud-observe on `wg0`), and `exporters.scrapeAddress` overrides the address Prometheus should dial (cloud-observe advertises its WireGuard address).

**Server** (`prometheus/server.nix`): the interesting trick is compile-time service discovery. The port list of monitored exporters is a fixed attrset, and for each exporter a scrape job is built by filtering **all** `nixosConfigurations` for hosts where that exporter's option is `true`:

```nix
mkScrapeJob = exporterName: port: {
  job_name = "${exporterName}-exporter";
  scrape_interval = "30s";
  static_configs = /* filter nixosConfigurations by exporters.<name>.enable,
                      target = scrapeAddress or getHostAddress hostname,
                      labels.hostname = hostname */;
};
```

So enabling `prometheus.exporters.node.enable = true` in a host file simultaneously opens its firewall, runs the exporter, **and** adds it to Prometheus's targets on the next rebuild — targets and labels (`hostname`) come from the configs themselves. Prometheus is served on 9090 behind nginx at `prometheus.<domain>`.

## Alert pipeline: everything → ntfy

**ntfy** (`ntfy/default.nix`, on cloud-observe): `deny-all` auth + login, config via agenix env file, bound to localhost behind nginx at the public `ntfy-server` endpoint. Known topics in `consts.ntfy-topics`: `prometheus-alerts`, `gatus-alerts` (plus `harmonia-alerts`, `trivy` used by non-observability modules).

**Prometheus path** (`prometheus/server.nix`), all wrapped in `lib.mkIf ntfyEnabled` where:

```nix
ntfyEnabled = helpers.anyHostEnabled nixosConfigurations [ "custom" "services" "observability" "ntfy" "enable" ];
```

- Three built-in rules: `HostDown` (`up{job="node-exporter"} == 0`, 5m, critical), `HostLowDiskSpace` (<10% on `/`), `HostLowMemory` (<10% available).
- Alertmanager (localhost:8000) routes everything to one receiver whose webhook hits an **`alertmanager-ntfy`** publisher sidecar (`services.prometheus.alertmanager-ntfy`), which translates alerts to ntfy notifications on topic `prometheus-alerts`, priority `high` while firing, title/description from the alert annotations. Its publisher config is an agenix secret. Grouping: `alertname`+`hostname`, repeat every 6h, `send_resolved = true`.

**Gatus path** (`gatus/default.nix`, on cloud-observe): external uptime + synthetic probes, and it consumes the same cross-host trick:

- `helpers.getEnabledServices` maps `custom.services.*` enable flags to service names per host; Gatus iterates **every** host, builds an HTTPS check per enabled service (`https://<subdomain>.<domain>/`, expecting 2xx/3xx; a `servicePolicies` table overrides per-service route/path/conditions, e.g. ntfy checks `/v1/health`, krawl a honeypot path).
- Private services are resolvable because Gatus injects `networking.extraHosts` entries mapping each host's internal address to its service FQDNs.
- Plus fixed infra checks: ntfy public health (5m) and `icmp://` to the home WireGuard gateway (1m).
- Alerts (fail 3 / success 2, send-on-resolved) go to ntfy topic `gatus-alerts` via the local ntfy, token from an agenix env file.

**Local/other monitors**: Beszel is a lightweight push-style server monitor (agent → hub at `https://beszel.<domain>`, SSH public-key auth from `lib/keys.nix`; the agent unit is force-run as root with `ProtectSystem=strict` for disk/podman stats). Monit is a self-contained local watchdog (disk >90%, nginx/monit process, syslog alerts) — module exists, no host enables it.

## Dashboards: Grafana (`grafana/default.nix`)

- **Hard asserts** that Prometheus server and Loki server are enabled on the same host — Grafana refuses to build without its two datasources.
- Datasources provisioned as code with stable uids `prometheus` / `loki`, both proxied over localhost.
- Dashboards are **pinned at build time**: `pkgs.fetchurl` of specific grafana.com dashboard revisions (node, nginx, podman, restic, smartd, wireguard, libvirt; crowdsec one is commented out), then a `runCommand` "sanitizes" datasource placeholders (`${DS_PROMETHEUS}` → `prometheus`) with sed, plus a jq patch fixing the nginx exporter's `instance` template variable. A hand-written `systemd-logs.json` covers the Loki side.
- `security.secret_key` comes from agenix (`$__file{...}`); analytics/update checks off; timezone from `consts.timeZone`; served behind nginx at `grafana.<domain>`; plugin/log dirs excluded from restic.

## House conventions worth copying

- **Everything centralized**: ports, subdomains, OCI UIDs, ntfy topics, endpoints in `lib/consts.nix`; nothing hardcodes `:9090` or a hostname.
- **Cross-host composition via `nixosConfigurations`** (`anyHostEnabled`, `mkScrapeJob`, Gatus's endpoint list): the fleet's monitoring config is derived from what the fleet actually enables — adding a host or service needs no change in any monitoring module. This is the repo's answer to an inventory file.
- **Graceful degradation**: alert wiring (rules, alertmanager, Gatus alerts) is conditional on ntfy existing *somewhere*, so the stack is testable without the cloud node.
- **Fail-fast assertions** everywhere (empty interface names, missing secret envFiles, exporter prerequisites).
- **OCI container conventions** followed for Dockhand, NetAlertX, Portainer, Termix, Scanopy, podman-exporter: `helpers.mkOciUser` + `systemd.tmpfiles.rules` ownership, localhost port binds + `helpers.mkVirtualHost` nginx front, `io.containers.autoupdate=registry` labels, agenix for env/certs.
- **Backups**: `custom.services.infra.restic.extraExcludes` for Loki data, Prometheus stateDir, Grafana plugins/logs — dashboards and pipelines are reproducible from Nix, so only state that *isn't* reproducible gets backed up.

## Gaps / observations

- Loki is single-node, unauthenticated, filesystem-backed — fine for one hub, no HA or long retention story (`max_age = 12h` on the agent read side only).
- Only 3 Prometheus alert rules exist; everything subtler depends on eyeballing Grafana or on Beszel/Gatus.
- `monit`, `portainer`, `scanopy` modules and the crowdsec exporter are dead weight currently (no host enables them); scanopy even carries per-host daemon secrets (`secrets/observability/scanopy/daemon-*-env.age`).
- Workstations (`desktop`, `framework`) only export SMART — no node exporter, no Loki agent, so they're invisible to Grafana logs/metrics.
