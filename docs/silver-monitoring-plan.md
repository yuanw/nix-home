# Plan: Bring yomaq/nixos-config Monitoring Ideas to `silver`

Draft based on the findings in [yomaq-nix-config-monitoring.md](./yomaq-nix-config-monitoring.md) and [nixos-config-observability.md](./nixos-config-observability.md). `silver` is the always-on colmena-managed telemetry box (Cockpit + PCP today, see `hosts/silver/configuration.nix`), so it is the natural first node to grow a real alert/report layer on.

## What silver has today

- `services.cockpit-local` + `services.pcp` (from `modules/cockpit.nix`, `modules/services/monitoring/pcp.nix`) — interactive dashboards only.
- Nothing pushes out: no ntfy, no failure alerts, no upgrade reporting, no central logs. If `pmcd` dies at 3am, nobody notices until someone opens Cockpit.

## Ideas selected (and where they come from)

| # | Idea | Source | Why it fits silver |
|---|---|---|---|
| 1 | `lastUpdate.html` version blob | yomaq | ~15 lines, zero deps; instantly answers "is silver running a stale closure?" across the colmena fleet |
| 2 | ntfy publisher base ("delivery only") | yomaq | one shared option set (`url`/`topic`/`priority`) + helper script that every later publisher reads |
| 3 | smartd → ntfy | yomaq | smartmontools already installed; the 2014 SSD deserves life-percentage/reallocation alerts |
| 4 | systemd-failure reporter with LLM triage + fallback | yomaq | the centerpiece; dgx-spark already runs vLLM (`services.vllm.instances.*`) so the "clever part" has a home, and the plain-log fallback means a dead LLM never silences alerts |
| 5 | Staleness push ("missing-a-push is the alert") | yomaq `autoUpgradeNix` | weekly timer: flake `lastModified` vs running system → ntfy if silver hasn't been rebuilt in N days |
| 6 | Alloy → Loki journal shipping | nixos-config | optional/last — only worth it once a Loki server exists somewhere |
| 7 | Central PCP view (pmproxy fan-out + Grafana/grafana-pcp) | PCP-native | answers "see every machine's metrics in one place" without adopting the Prometheus stack |

## Ideas deliberately NOT copied

- **Glances + Homepage widgets** — duplicates PCP/Cockpit, which is this repo's house metrics stack. Replacing PCP with Prometheus is out of scope.
- **Prometheus exporters / server / Grafana** (nixos-config) — no aggregator host chosen; exporters without a server are dead weight. If we ever want them, steal nixos-config's `mkScrapeJob` (filter `nixosConfigurations` for `exporters.<name>.enable`) which beats yomaq's hand-maintained `inventory.nix`. (Grafana still appears in Phase 7, but only as a PCP *viewer* — no Prometheus datasource.)
- **Gatus as the alerting brain** — needs an aggregator VM + probes; overkill for 2–3 servers. The ntfy push in #5 gives the same "batch job verdict" value without the server. Revisit when the NixOS fleet is ≥4 machines.
- **Monit** — the failure reporter (#4) covers disk/process alerts better.

## Decisions to make first

1. **ntfy hosting** (blocks #2–#5):
   - **(a) `ntfy.sh` hosted** — zero infra, phone already can subscribe; security = unguessable topic, secrets via agenix. *Recommended starting point.*
   - (b) self-host `services.ntfy-sh` on silver + make silver a tailnet node (`services.tailscale.enable`, like `hosts/mist.nix`) and subscribe the phone over the tailnet — the yomaq shape, more moving parts.
2. **Triage LLM endpoint**: which vLLM instance/model on dgx-spark serves triage requests, and its URL. Fallback path must work when it's down.
3. **Where the Loki server lives** (only if #6 is picked): misfit is the better home (always-on, already has caddy); silver can just run the agent.
4. **Which host is the PCP collector** (only if #7 is picked): silver fits the role (it's the telemetry box, always-on); dgx-spark is the fallback since it already has pmproxy.

## Phase 1 — Version blob (no deps, do first)

New: `modules/services/monitoring/version-blob.nix`, option `services.version-blob.enable`.

- `system.activationScripts` writes `/var/lib/version-blob/lastUpdate.html`: `{"date": ..., "commit": "<inputs.self.shortRev>", "hostname": ...}` — emit **only** when `inputs.self ? rev` (yomaq: a dirty tree never claims a version). Needs `inputs` in `specialArgs` (already passed through `hosts/colmena.nix`).
- Serve with `services.static-web-server` on port 8787, firewall open to the LAN only.
- **Verify:** `nix eval .#colmenaHive.nodes.silver...` then `curl http://silver:8787/lastUpdate.html` after `just remote-apply silver`.

## Phase 2 — ntfy publisher base

New: `modules/services/monitoring/ntfy.nix`, options `services.ntfy-notify = { url, topic, tokenFile? }` + a small generated `ntfy-send` script (`curl -H "Title: …" -H "Priority: …" -H "Tags: …" "$URL/$TOPIC"`) on `PATH`. All later publishers call `ntfy-send`, nobody hardcodes URLs (nixos-config's centralization lesson; topic/token land in agenix per `secrets/` convention).

## Phase 3 — smartd → ntfy

In the same module or a small `smartd-notify.nix`:

- `services.smartd.enable`, `MailAddress` disabled, `-M exec <script>` where the script pipes `$SMARTD_MESSAGE` to `ntfy-send -t "SMART: $(hostname)" -p high --tags warning,disk` (yomaq's exact pattern, mail/wall off).
- **Verify:** `smartd -d -r test` style dry-run on the box (manual).

## Phase 4 — Failure reporter with LLM triage

New: `modules/services/monitoring/failure-reporter.nix`, options `services.failure-reporter.services.<name>.priority`, default list: `pcp-pmcd`, `pcp-pmproxy`, `cockpit`, `sshd`, `version-blob` static server.

Per yomaq's `serviceFailureReports`, per monitored unit inject `unitConfig.OnFailure = [ "monitor@%i.service" ]`; template unit `monitor@.service`:

1. Counter file `/var/lib/failure-reporter/<svc>.state`; act only at `FAILURES >= threshold`, where **threshold = `StartLimitBurst` if the unit has `Restart=always|on-failure`, else 1** (derived, not magic numbers).
2. `journalctl -u <svc> -n 25`, control-char escaped.
3. POST to the decision-#2 vLLM endpoint (OpenAI-compatible `/v1/chat/completions`) with prompt: *"short summary of the issue, 3 potential solutions, then the last 5 log lines"*.
4. `ntfy-send` the answer, `Title: <svc> on silver failed`.
5. **Fallback:** any LLM error → send the raw log tail instead; a dead LLM must not silence the alert.
6. Reset counter.

**Verify:** `systemctl start monitor@<a-broken-dummy>.service` (or ` systemd-analyze verify`), confirm a phone notification with and without vLLM running.

## Phase 5 — Staleness push

`modules/services/monitoring/staleness.nix`: weekly `systemd.timer` runs a script that compares `nix flake metadata <repo> --json | jq .lastModified` against the closure date silver booted; if > `maxAgeDays` (default 14) → `ntfy-send -p high "silver stale: <N> days"`. Absence of the weekly push is itself the canary (yomaq).

## Phase 6 (optional) — Journal → Loki

Copy nixos-config's `loki/agent.nix` idea verbatim: `services.alloy` with `loki.source.journal` (max_age 12h) relabeling `unit`/`host`/`job=systemd-journal`, pushing to a Loki server option (default misfit, port 3100). Only build this when the Loki server decision is made; Cockpit's journal view covers per-node needs until then.

## Phase 7 (optional) — Central PCP view across all machines

No new metrics pipeline — PCP's own fan-out, wired into the existing `modules/services/monitoring/pcp.nix`:

- **Module change**: add a `collector` preset (or a `services.pcp.collectHosts = [ "dgx-spark" "misfit" ... ]` option) that renders `hosts = ...` into `pmproxy.conf`, and enable `services.redis-server` (pmproxy's pmseries/Vector REST API is redis-backed). Fan-out pmproxy scrapes each host's pmcd live; pmlogger archives stay local on each machine (that is the right split — logs don't move, queries do).
- **Fleet changes**: every metric-producing host keeps `services.pcp` but moves to `preset = "minimal"` (pmcd only) and lists the collector's network in `allowedNetworks`; only the collector runs pmproxy/pmlogger/pmie. `misfit` currently runs no PCP at all — it would need `minimal` too.
- **Viewing**: `services.grafana` on the collector + provisioned **grafana-pcp** datasource (`pcp-vector` pointing at `localhost:44322`) + the official PCP Vector dashboards, which carry a `hostspec` picker — one dashboard, all machines.
- **Check before committing**: whether the repo's vendored `pcp` package (`packages/` overlay, re-added after upstream removal) includes the pmproxy web/redis bits; if `vector` (PCP's own web UI, no Grafana needed) ships with it, prefer that over Grafana until a dashboard is actually missed.
- **Verify**: `pmval kernel.all.load -h dgx-spark` from the collector (cross-host live query), then the Grafana dashboard shows both hosts.

## Order, effort, risk

| Phase | ~Effort | Risk / note |
|---|---|---|
| 1 version blob | 1 h | none, additive |
| 2 ntfy base | 1–2 h | needs decision #1 |
| 3 smartd | 1 h | verify script quoting on target |
| 4 failure reporter | 3–4 h | most moving parts; fallback must be tested explicitly |
| 5 staleness | 1 h | flake URL/rev access from the box (private input — may need the forgejo URL + token) |
| 6 Loki | 2 h | blocked on decision #3 |
| 7 PCP collector + Grafana | 3–4 h | blocked on decision #4; needs pmproxy host-spec support verified in the vendored pcp package |

All builds/deploys stay manual per this repo's rules: agent may `nix eval`; operator runs `just remote-apply silver` (buildOnTarget).

---

*Skipped/not verified: didn't confirm the vLLM endpoint on dgx-spark actually exposes a chat-completions model reachable from silver; Phase 5's flake-metadata step against the private git input is untested; nothing here was evaluated with `nix eval` yet.*
