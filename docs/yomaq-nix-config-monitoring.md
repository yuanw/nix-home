# How yomaq-nix-config Monitors Many Machines with Homepage, ntfy and Gatus

Findings from reading `../yomaq-nix-config` (README, `inventory.nix`, `modules/hosts/{homepage,gatus,ntfy,glances,serviceFailureReports,autoUpgradeNix,smartd}`, `modules/hosts/zfs/*`, `modules/virtualization/{containers,microvms}/*`).

## TL;DR

- One file, `inventory.nix`, lists what every host runs. All monitoring config is **derived** from it, never hand-written per machine.
- Service modules contribute to three targets from the same place: the service itself, `yomaq.gatus.endpoints`, and `yomaq.homepage.groups.services`.
- **Gatus** is the alerting brain: it probes URLs outward, and accepts pushed results upward for things that can't be probed (OS upgrades, ZFS backups).
- **ntfy** is delivery only — Gatus's alert sink, plus direct publishers (SMART, ZED, an LLM-triaged systemd-failure reporter).
- **Homepage** is a read-only dashboard whose per-host widgets (CPU/RAM/disk/version) and service links are generated from the same inventory, so it grows as hosts are added.
- Everything aggregates on `azure`; other hosts just self-report. Tailscale DNS + HTTPS makes cross-machine probing work with no LAN assumption and no open ports.

## The mechanism: `inventory.nix` as the single source of truth

`modules/hosts/inventory/nixos.nix` defines `inventory.hosts` as an `attrsOf submodule` and imports the top-level `inventory.nix`:

```nix
azure = {
  users.enableUsers = [ "admin" ];
  docker.enable = true;
  syncoid.enable = true;
  syncoid.isBackupServer = true;
  microvms = [ "tsidp" "audiobookshelf" "ntfy" "gatus" "homepage" "nextcloud" "calibre" "forgejo" "immich" ];
  pods = { golink.enable = true; teslamate.enable = true; n8n.enable = true; /* ... */ };
};
teal  = { nixos-containers.code-server.enable = true; syncoid.enable = true; /* ... */ };
green = { microvms = [ "testvm" ]; /* ... */ };
```

Every monitoring-related module uses the same idiom — iterate **all** inventory hosts and filter on a flag — so the aggregating machine emits monitoring config for services that live on *other* machines:

```nix
builtins.filter (host: config.inventory.hosts.${host}.pods."${NAME}".enable or false)
  (builtins.attrNames config.inventory.hosts)
```

Consequence: enabling a service on any host in `inventory.nix` makes it appear in Gatus and Homepage with no other edits. That is the whole "monitors different machines" trick.

`modules/hosts/suites/foundation/nixos.nix` is the per-host on-switch. `yomaq.suites.foundation.enable = true` (set in every active host file) turns on:

```nix
autoUpgrade.enable; initrd-tailscale.enable; fwupd.enable;
monitorServices.enable; ssh.enable; sanoid.enable; timezone.central; smartd.enable;
inventory.hosts."${hostName}".glances.enable = true;
```

and asserts that the running hostname exists in the inventory, and that every inventory entry exists in `nixosConfigurations`. So a host can't silently escape monitoring.

## Three deployment shapes, one pattern

Each service module (docker pod, nixos-container, or microvm) has exactly three config blocks. Using `modules/virtualization/containers/docker/n8n.nix` as the canonical example:

| Block | Runs on | Purpose |
|---|---|---|
| `mkIf cfg.enable` | the host that runs the service | the container itself + `yomaq.monitorServices.services."docker-n8n".priority = "medium"` |
| `mkIf config.yomaq.gatus.enable` | the Gatus machine | one endpoint per inventory host where the pod is enabled |
| `mkIf config.yomaq.homepage.enable` | the Homepage machine | one service entry (icon / href / `siteMonitor`) per deploying host |

Addressing is uniform: containers are `https://<host>-<service>.<tailnet>.ts.net`, microvms are `https://<service>.<tailnet>.ts.net` (a microvm is its own tailnet node). Each unit runs a Caddy `reverse_proxy 127.0.0.1:<port>` and Tailscale serves DNS + a real cert. No LAN dependency, so a server can move networks unchanged.

Microvms split the config in two files, which is how the aggregation works across a VM boundary:

- `microvms/<name>/microvm.nix` — inside the VM (this is where `yomaq.gatus.enable = true` / `services.gatus.enable = true` live).
- `microvms/<name>/nixos.nix` — on the **parent** host, with two carefully separated booleans:
  - `vmDeployedOnThisHost` — this host runs the VM (used to deploy the agenix secret).
  - `vmDeployedOnHost` — *any* host runs the VM (used to add the Gatus endpoint / Homepage entry from the parent's config).

Host-side glue is `modules/hosts/microvms/nixos.nix`, which declares `inventory.hosts.<host>.microvms` and sets `microvm.autostart` from it.

## Gatus — the alerting brain

`modules/hosts/gatus/nixos.nix` is a 30-line pass-through: `yomaq.gatus.enable` plus `endpoints` and `externalEndpoints` lists, forwarded to `services.gatus.settings`. The server itself is configured in `modules/virtualization/microvms/gatus/microvm.nix` (duplicated in `containers/nixos-containers/gatus/nixos.nix`):

```nix
services.gatus.settings = {
  web.port = 8080;
  storage = { type = "sqlite"; path = "/var/lib/gatus/data.db"; };
  alerting.ntfy = {
    url = config.yomaq.ntfy.ntfyUrl;
    topic = config.yomaq.ntfy.defaultTopic;
    priority = 3;
    default-alert = { enabled = true; failure-threshold = 10; success-threshold = 10; send-on-resolved = true; };
  };
};
```

Details worth copying: `ExecStartPre = sleep 10` so the tailnet is up before probing starts; `/var/lib` persisted so sqlite history survives impermanence reboots; Caddy in front for HTTPS.

### Direction 1 — Gatus probes outward (`endpoints`)

Every service module adds, per deploying host:

```nix
{ name = "${host}-${NAME}"; group = "webapps";
  url = "https://${host}-${NAME}.${tailnet}.ts.net";
  interval = "5m"; conditions = [ "[STATUS] == 200" ];
  alerts = [ { type = "ntfy"; "failure-threshold" = 3; description = "healthcheck failed"; } ]; }
```

This is self-referential on purpose: `gatus/nixos.nix` monitors ntfy, `ntfy/nixos.nix` monitors gatus, and both monitor homepage. The list today covers the azure microvms (gatus, ntfy, homepage, nextcloud, immich, forgejo, calibre, audiobookshelf, tsidp) and the docker pods (n8n, dufs, golink, searxng, litellm, teslamate, changedetection, linkwarden, drop, bentopdf, foundry-vtt, ollama, open-webui).

### Direction 2 — hosts push up (`externalEndpoints`)

Things a URL probe cannot detect — a failed OS upgrade, a missed ZFS replication — report in passively. Gatus declares one token endpoint per inventory host:

```nix
# autoUpgradeNix/nixos.nix — group "updates", one per inventory host
{ name = "${host}"; group = "updates"; token = "${host}";
  alerts = [ { type = "ntfy"; "failure-threshold" = 3; description = "healthcheck failed"; } ]; }

# zfs/syncoid/nixos.nix — group "backup", one per host with syncoid.enable
```

and the target machine posts the verdict with systemd `onSuccess`/`onFailure` hooks, using the hostname as the bearer token:

```bash
curl -X POST ${config.yomaq.gatus.url}/api/v1/endpoints/updates_${hostName}/external?success=true\&error= \
     -H 'Authorization: Bearer ${hostName}'
```

`nixos-upgrade` gets this via `systemd.services.nixos-upgrade.onSuccess/onFailure`, and the upgrade itself is gated by an `ExecCondition` that compares `nix flake metadata` `lastModified` of the remote flake against `self` — so no needless rebuilds, and the *absence* of a success push is what Gatus alerts on. Syncoid does the same per backup client with `syncoid-success-<host>` / `syncoid-fail-<host>` units.

## ntfy — delivery only

`modules/hosts/ntfy/nixos.nix` is pure config, no service (the server lives in `microvms/ntfy/microvm.nix` with `upstream-base-url = https://ntfy.sh` and Caddy TLS):

```nix
yomaq.ntfy = {
  ntfyUrl = "https://ntfy.${config.yomaq.tailscale.tailnetName}.ts.net";
  defaultTopic = "ntfy";
  defaultPriority = "p:3";
};
```

Everyone that notifies reads these options. Besides being Gatus's alert sink, ntfy has direct publishers that bypass Gatus entirely:

- **SMART** (`modules/hosts/smartd/nixos.nix`) — `services.smartd.defaults.autodetected = "-a -o on -s (S/../.././02|L/../../7/04) -M exec <generated script>"`, where the script curls with `Title: SMART Error on $(hostname)`, `Priority: high`, `Tags: warning,disk`, `-d "$SMARTD_MESSAGE"` to `ntfy/<ntfyTopic>` (default topic `zfs`). Mail and wall notifications are disabled.
- **ZED / ZFS events** (`modules/hosts/zfs/disks/nixos.nix`) — `services.zfs.zed.settings = { ZED_NTFY_TOPIC = "zfs"; ZED_NTFY_URL = "https://ntfy.<tailnet>.ts.net"; }`, `enableMail = false`.
- **`yomaq.monitorServices`** — the LLM triage reporter, next section.

## The systemd-failure reporter with LLM triage

`modules/hosts/serviceFailureReports/nixos.nix`. For each name in `yomaq.monitorServices.services`, it injects `onFailure = [ "yomaq-monitor@%i.service" ]` into that unit, and drops a per-service env file in `/var/lib/systemd/yomaq-monitor/<svc>.conf`.

The template unit:

1. Increments a failure counter in `<svc>.state`.
2. Only once `FAILURES >= SERVICE_RESTART_THRESHOLD` does it act — the threshold is **derived from the unit itself**: `1` if the service has no `Restart=always|on-failure`, else its `StartLimitBurst` (default 5). A unit that doesn't self-heal alerts immediately; a flappy one has to exhaust its restart budget first.
3. Grabs `journalctl -u <svc> -n 25`, control-char-escapes it.
4. Sends the logs to **Ollama** (`https://wsl-ollama.<tailnet>.ts.net/api/generate`, model `ornith:9b`) with the prompt *"short summary of the issue, 3 potential solutions, then the last 5 log lines"*.
5. POSTs the model's answer to `${ntfyBaseUrl}/$SERVICE_TOPIC` with `Title: <svc> on <host> failed` — and falls back to a bare "Analysis failed" notification if Ollama is unreachable, so a dead LLM never silences the alert.
6. Resets the counter.

An `n8nUrl` option short-circuits 4–5 and POSTs `{service, host, logs, priority}` to an n8n webhook instead.

Service names come from elsewhere: `autoUpgradeNix` registers `nixos-upgrade` at `priority = high`; every docker/nixos-container module registers `docker-<NAME>` or `container@<host>-<NAME>` at `medium`.

## Homepage — a dashboard that generates itself

`modules/hosts/homepage/nixos.nix` exposes collector options that other modules append to:

```nix
yomaq.homepage.groups.services.<name> = { icon = "mdi-list-status"; href = ...; siteMonitor = ...; };
yomaq.homepage.groups.bookmarks.<name> = { ... };
```

and translates the plain attrs into Homepage's list-of-single-key-maps YAML shape with two local helpers (`yamlReadyList`, `formatWidgets`) before handing them to `services.homepage-dashboard`. Uses `pkgs.unstable.homepage-dashboard`; secrets via agenix `secrets/homepage.age` as `environmentFile`, giving `{{HOMEPAGE_VAR_*}}` (dashboard title, AdGuard creds).

### Per-host health widgets, generated per inventory host

`modules/hosts/glances/nixos.nix` is the big one. Each host in the inventory with `glances.homepageMonitor = true` gets a row of widgets, built by `map` over the inventory:

| Widget | Source |
|---|---|
| CPU, MEMORY, Processes | `type = "glances"`, `url = http://<host>.<tailnet>.ts.net:61208`, `metric = cpu / memory / process` |
| PersistSave | same, `metric = fs:/persist/save` — tracks the impermanence persistence volume filling up |
| Version (Current Commit) | `type = "customapi"`, `http://<host>.<tailnet>.ts.net:8787/lastUpdate.html` → `date` + `commit` |

The version widget is how you see at a glance which machine is running an old flake revision: the glances module's `system.activationScripts.lastUpdate` writes `{"date": ..., "commit": "<inputs.self.shortRev>"}` into a directory served by `services.static-web-server` on 8787. It's only emitted `lib.mkIf (inputs.self ? rev)`, so a local dirty tree never claims a version.

Glances itself runs as a hand-written oneshot-style systemd service (`glances -w`, `DynamicUser`, `Restart = on-failure`) with a large generated config symlink in via `tmpfiles` `L+ /etc/glances/glances.conf` — thresholds tuned (cpu 65/75/85, mem 50/70/90, fs 50/70/90, `temperature_hdd` 45/52/60), and the noisy plugins (`percpu`, `gpu`, `load`, `network`, `diskio`, `smart`, `wifi`, ...) disabled.

`containers/*/nixos.nix` and `microvms/*/nixos.nix` set `inventory.hosts.<name>.glances.homepageMonitor = false` for their own inventory entries — VMs and containers aren't machines, so they don't get a host widget row.

### Flake / repo widgets

A `Flake` group queries the Forgejo API via `customapi` widgets: last commit date, last commit message, `flake.lock` last-update date, and the currently-running SHA from `https://homepage.<tailnet>.ts.net/forgejo-stats/commit.json`. That JSON is produced on the homepage VM by a `fetch-forgejo-sha` systemd timer (every 5 min, `jq '{sha: .[0].sha[0:8]}'`) and served by Caddy's `file_server` out of `/var/lib`.

### Layout, also generated

`yomaq.homepage.settingsLayout` is built by the same inventory `map`: the `Flake` group plus one entry per monitored host, each `{ icon = "mdi-server"; tab = "Glances"; style = "row"; columns = 5; }`. The `Services` tab comes from `groups.services` with `settingsLayout.Services = { tab = "Services"; }`. Non-monitoring modules push into the same collector — e.g. `modules/hosts/adGuardHome/nixos.nix` appends an AdGuard widget entry.

Fixed chrome (in `microvms/homepage/microvm.nix`): `HOMEPAGE_ALLOWED_HOSTS` env, `statusStyle = "dot"`, dark/slate, blurred background, `useEqualHeights`, a custom search widget pointed at the searxng pod, and an openmeteo weather tile.

The module's own readme (`modules/hosts/homepage/readme.md`) states the goal and the known cost:

> Goal here is to automatically generate homepage configuration as I deploy services across different hosts... Biggest loss with this setup is I don't have control over ordering, other than alphabetically.

## Topology today

| Machine | Monitoring role |
|---|---|
| `azure` | **The aggregator.** Hosts the `gatus`, `ntfy` and `homepage` microvms, plus most pods (n8n, searxng, teslamate, golink, dufs, changedetection, linkwarden, open-webui, litellm stack, drop, bentopdf, palworld). Also `syncoid.isBackupServer`. |
| `teal` | NixOS container `code-server`; syncoid client. |
| `green` | microvm `testvm`; syncoid client. |
| `sage` | palworld pod; syncoid client. |
| `wsl` | ollama / speaches / litellm pods; hosts the Ollama that triages failure logs; `syncoid.enable = false`. |
| `vermilion`, darwin `pewter`/`midnight` | desktops — `suites.foundation` only (glances, smartd, monitorServices, autoUpgrade push). |
| commented out | `smalt`, `jade`, `moss`, `pearl` (and `hosts/old/blue`, `carob`). |

## What's worth stealing

- **Inventory-driven monitoring.** `filter` + `map` over `attrNames inventory.hosts` beats a per-host monitoring list; a service can't be deployed without being monitored, because monitoring is derived from the same flag as the deployment.
- **Bidirectional Gatus.** URL probes for web things, `externalEndpoints` with `token = hostname` + systemd `onSuccess`/`onFailure` for batch jobs and upgrades. Missing-a-push is the alert.
- **Threshold derived from `StartLimitBurst`** in the failure reporter — no magic number per service.
- **LLM triage with a plain-log fallback**, so the clever part failing degrades to a normal alert.
- **`lastUpdate.html` written by an activation script** — a one-line JSON blob per host is enough for "which machine is stale".
- **Two booleans at the VM boundary** (`vmDeployedOnHost` vs `vmDeployedOnThisHost`) so a VM can register itself with the aggregator from its parent's config while secrets only land on the parent.

## Gaps / caveats

- The gatus and homepage server config is **duplicated** between `microvms/<name>/{microvm,nixos}.nix` and `containers/nixos-containers/<name>/nixos.nix` — the README lists "migrate nixos-containers to microvms" as a TODO, so this is a known temporary state.
- Gatus alert config is copy-pasted per module (`type = "ntfy"; failure-threshold = 3`) rather than relying solely on `default-alert`; changing it means touching ~30 files.
- Homepage ordering is alphabetical only (acknowledged in its readme).
- The `container` suite for gatus binds the whole `/var/lib` to a host path to keep sqlite alive, with a comment apologising for it.
- ntfy has no auth in this repo and `upstream-base-url = https://ntfy.sh`, so published topics are public-by-guess — access control rests on the tailnet and Tailscale ACLs (managed separately in `yomaq/Tailscale-ACL`).
- I read only the nix source; I did not verify any of this against a running Gatus/Homepage instance.
