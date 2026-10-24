host := `hostname -s`
pipe_feature := `nix --version 2>/dev/null | grep -qi lix && echo pipe-operator || echo pipe-operators`
nix_config := `feature=$(nix --version 2>/dev/null | grep -qi lix && echo pipe-operator || echo pipe-operators); printf 'extra-experimental-features = nix-command flakes %s' "$feature"`
nix := `feature=$(nix --version 2>/dev/null | grep -qi lix && echo pipe-operator || echo pipe-operators); printf "env NIX_CONFIG='extra-experimental-features = nix-command flakes %s' nix" "$feature"`

# Workiva tools are flake packages from nix-home-private (see devshell.nix).
# Sibling checkout layout is the default inside those packages; this file only
# names the commands.
# list all commands
default:
    @just --list

# prefetch Workiva git sources (needed before build on work hosts)
prefetch-work-sources:
    @{{nix}} run .#prefetch-work-sources

# build os
build:
    @if [ "{{lowercase(host)}}" = "wk01174" ]; then \
        {{nix}} run .#prefetch-work-sources; \
        {{nix}} run .#nix-build-with-workiva-netrc -- ".#{{lowercase(host)}}"; \
    else \
        {{nix}} build --quiet --fallback ".#{{lowercase(host)}}"; \
    fi

update-all:
    @{{nix}} flake update

update INPUT:
    @{{nix}} flake update {{INPUT}}

# update ad-hoc packages upstream references 
nix-update:
    @nix-update -f ./packages/release.nix agent-shell --src-only --version=branch
    @nix-update -f ./packages/release.nix acp --src-only --version=branch --override-filename ./packages/emacs/acp.nix
    @nix-update -f ./packages/release.nix auto-save --src-only --version=branch
    @nix-update -f ./packages/release.nix caveman --src-only --version=branch
    @nix-update -f ./packages/release.nix cursor-agent-acp --src-only
    @nix-update -f ./packages/release.nix pi-acp --src-only --version=branch --override-filename ./packages/pi-acp.nix
    @nix-update -f ./packages/release.nix consult-omni --src-only --version=branch
    @nix-update -f ./packages/release.nix emacs-skills --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel-agent --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel-quick --src-only --version=branch
    @nix-update -f ./packages/release.nix humanizer --src-only --version=branch
    @nix-update -f ./packages/release.nix i-have-adhd --src-only --version=branch
    @nix-update -f ./packages/release.nix hurl-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix hel --src-only --version=branch --override-filename ./packages/emacs/hel.nix
    @nix-update -f ./packages/release.nix hel-leader --src-only --version=branch --override-filename ./packages/emacs/hel-leader.nix
    @nix-update -f ./packages/release.nix hel-ghostel --src-only --version=branch --override-filename ./packages/emacs/hel-ghostel.nix
    @nix-update -f ./packages/release.nix hel-collection --src-only --version=branch --override-filename ./packages/emacs/hel-collection.nix
    @nix-update -f ./packages/release.nix knockknock --src-only --version=branch
    @nix-update -f ./packages/release.nix lean4-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-racket --src-only --version=branch
    @nix-update -f ./packages/release.nix shell-maker --src-only --version=branch --override-filename ./packages/emacs/shell-maker.nix
    @nix-update -f ./packages/release.nix pilish --src-only --version=branch --override-filename ./packages/emacs/pilish.nix
    @nix-update -f ./packages/release.nix md-ts-mode --src-only --version=branch --override-filename ./packages/emacs/md-ts-mode.nix
    @nix-update -f ./packages/release.nix markdown-table-wrap --src-only --version=branch --override-filename ./packages/emacs/markdown-table-wrap.nix
    @nix-update -f ./packages/release.nix thrift-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ultra-scroll --src-only --version=branch
    @nix-update -f ./packages/release.nix pi-cursor-agent --src-only --override-filename ./packages/pi-extensions/pi-cursor-agent/default.nix --version-regex 'pi-cursor-agent@(.+)'
    # Vendored lock: bump version/src; also refresh npmDepsHash. If production deps change, regenerate packages/pi-extensions/pi-interactive-shell.package-lock.json first.
    @nix-update -f ./packages/release.nix pi-interactive-shell --override-filename ./packages/pi-extensions/pi-interactive-shell.nix --version-regex 'v(.*)'
    # Both are rev-pinned (no tags) from https://github.com/rytswd/pi-agent-extensions
    @nix-update -f ./packages/release.nix pi-permission-gate --src-only --version=branch --override-filename ./packages/pi-extensions/pi-permission-gate.nix
    @nix-update -f ./packages/release.nix pi-slow-mode --src-only --version=branch --override-filename ./packages/pi-extensions/pi-slow-mode.nix
    @nix-update -f ./packages/release.nix pi-ponytail --src-only --version=branch
    @nix-update -f ./packages/release.nix tccutil --src-only
    @nix-update -f ./packages/release.nix ds4 --src-only --version=branch

# Take the newest stable 1Password into packages/_1password-gui/sources.json,
# so a host does not wait on nixpkgs to notice a release.  Idempotent: it
# rewrites only the os/arch pairs that file already lists.
bump-1password:
    @./scripts/bump-1password

# Regenerate the Workiva pins from upstream, then warm the store.
update-wk:
	@{{nix}} run .#nvfetcher-work-sources
	@{{nix}} run .#bump-semver-git-sources
	just prefetch-work-sources

# Run colmena against one host from the hive, with an ssh agent available to the
# target (private flake inputs get fetched there when buildOnTarget is set).
colmena-run ACTION HOST:
    @set -e; \
    if [ "$(uname)" = "Darwin" ]; then \
        ssh-add -l 2>/dev/null || ssh-add --apple-use-keychain ~/.ssh/id_ed25519; \
        colmena {{ACTION}} --on {{HOST}}; \
    else \
        eval `ssh-agent -s`; \
        trap 'ssh-agent -k >/dev/null' EXIT; \
        setsid ssh-add ~/.ssh/id_ed25519 < /dev/null; \
        colmena {{ACTION}} --on {{HOST}}; \
    fi

# build the closure on a remote host (no activation).  HOST is a hive name:
# just remote-build misfit | dgx-spark | asche
remote-build HOST:
    @just colmena-run build {{HOST}}

# build + activate on a remote host
remote-apply HOST:
    @just colmena-run apply {{HOST}}

# kept so docs/redsnow-native-plan.org and muscle memory keep working
colmena-spark-build:
    @just remote-build dgx-spark

colmena-spark-apply:
    @just remote-apply dgx-spark

# The closures the LAN binary cache keeps warm (docs/harmonia-build-hub.org D5).
cache-targets := "silver misfit dgx-spark mist"
silver-ssh := "yuanw@silver.lan"

# Build every host closure on silver's LAN builders and root it there.
cache-build:
    @set -e; \
    for t in {{cache-targets}}; do \
        echo "== $t"; \
        just cache-build-one "$t"; \
    done

# Build one host closure on the builders and root it on silver.
#
# The Mac evaluates — it is the only box with the private flake input — and
# copies just the .drv closure over; silver's daemon realises it with
# --max-jobs 0, which is what forces every missing drv out to a builder
# instead of compiling on silver.  `nix build <drv>` is not used because on
# the Spark it exits 0 without building when handed a .drv path (measured
# 2026-10-23); `nix-store -r` is the reliable remote-store form.
#
# The root lives under /nix/var/nix/gcroots/cache/<host>, which the standard
# gcroots scan covers, so Harmonia's GC keeps it.  Drop a target from
# cache-targets and its root has to be deleted by hand.
cache-build-one HOST:
    @set -e; \
    drv=$({{nix}} eval --raw ".#packages.{{HOST}}.drvPath"); \
    echo "drv: $drv"; \
    {{nix}} copy --derivation --to ssh-ng://{{silver-ssh}} "$drv"; \
    ssh {{silver-ssh}} "nix-store --realise '$drv' --max-jobs 0 --keep-going \
        --add-root /nix/var/nix/gcroots/cache/{{HOST}} --indirect"

# what is rooted, and how big the store and the free space got
cache-status:
    @ssh {{silver-ssh}} 'ls -l /nix/var/nix/gcroots/cache/ 2>/dev/null; df -h / | tail -1; du -sh /nix/store'

# build and deploy to local host (macOS or NixOS)
switch:
    @if [ "$(uname)" = "Darwin" ]; then \
        sudo env NIX_CONFIG="{{nix_config}}" darwin-rebuild switch --flake . --fallback; \
    else \
        env NIX_CONFIG="{{nix_config}}" nixos-rebuild switch --flake '.#{{lowercase(host)}}' --quiet --sudo --fallback; \
        if systemctl is-enabled --quiet emacs.service 2>/dev/null; then sudo systemctl try-restart emacs.service; fi; \
    fi

# print the generated nima default.el content
nima-emacs-print-config:
    @set -e; \
    if [ "$(uname)" = "Darwin" ]; then \
        host_expr='flake.darwinConfigurations."{{host}}"'; \
    else \
        host_expr='flake.nixosConfigurations."{{lowercase(host)}}"'; \
    fi; \
    {{nix}} eval --impure --raw --expr "let flake = builtins.getFlake \"path:{{justfile_directory()}}\"; host = ${host_expr}; pkgs = host.pkgs; nima = import {{justfile_directory()}}/modules/editor/emacs/nima.nix { inherit pkgs; myConfig = host.config.my; emacsConfig = host.config.modules.editors.emacs; rawOutput = true; }; in nima.config.defaultEl.content"


# build devshell + system and push both closures to cachix
push-all:
    @bash -o pipefail -c 'env NIX_CONFIG="{{nix_config}}" nix build --no-link --print-out-paths .#devShells.$(env NIX_CONFIG="{{nix_config}}" nix eval --impure --raw --expr builtins.currentSystem).default .#{{lowercase(host)}} | xargs -n1 cachix push yuanw-nix-home-macos'

sys-diff:
    @nix store diff-closures /run/current-system ./result

# Build on dgx-spark by pointing Nix at its store directly. This Mac evaluates
# the flake -- dirty working tree and local inputs included -- and only the
# derivations and results travel, so there is no rsync of the tree and
# nix-home-private never has to exist on the Spark.
spark_store := "ssh-ng://yuanw@dgx-spark.local"

# The Spark also serves tensorfold-zig (~100 GiB resident); an unbounded kernel
# rebuild OOM-killed the model on 2026-10-14. Cap it. Raise --cores if the model
# is stopped (or point --store at another aarch64-linux builder).
spark_build := "--eval-store auto --max-jobs 1 --cores 6"

# where mono-gateway-serve parks the image on the Spark for the board to curl
mono-work-dir := "nix-mono"

# build mono-gateway rootfs image on dgx-spark (native aarch64-linux).
# The result stays in the Spark's store; fetch it with mono-gateway-fetch.
mono-gateway-build:
    @{{nix}} build '.#packages.aarch64-linux.rootfsImage' --store {{spark_store}} {{spark_build}} -L

# copy the image into this machine's store and point ./result-mono-gateway at it
mono-gateway-fetch:
    @P=$({{nix}} build '.#packages.aarch64-linux.rootfsImage' --store {{spark_store}} {{spark_build}} --no-link --print-out-paths); \
     {{nix}} copy --from '{{spark_store}}' "$P" && ln -sfn "$P" ./result-mono-gateway && echo ./result-mono-gateway

# python >= 3.11 http.server 404s on a symlink out of the served dir, so park a
# dereferenced rootfs.img for the mono to curl in Recovery Linux
mono-gateway-serve:
    @P=$({{nix}} build '.#packages.aarch64-linux.rootfsImage' --store {{spark_store}} {{spark_build}} --no-link --print-out-paths); \
     ssh -t yuanw@dgx-spark.local "mkdir -p {{mono-work-dir}} && cp -Lf '$P' {{mono-work-dir}}/rootfs.img && cd {{mono-work-dir}} && exec python3 -m http.server 8000"



# Address the board answers at. Since the swap it is the house router, so
# 192.168.1.1 -- the staging 10.0.0.2 went away with the fold (generation 16).
# For a board cabled straight to this Mac, override it: mono-gateway-host=10.0.0.2.
mono-gateway-host := "192.168.1.1"

# Build the gateway's NixOS system closure on dgx-spark and switch the running
# board. Cheaper than re-flashing: the closure reuses the ~561 store paths already
# on the eMMC, so a config change normally moves a few kilobytes. The copy is
# relayed through this Mac because dgx-spark has no route to 10.0.0.0/24.
mono-gateway-redeploy:
    @P=$({{nix}} build '.#packages.aarch64-linux.mono-gateway-system' --store {{spark_store}} {{spark_build}} --no-link --print-out-paths); \
     {{nix}} copy --from '{{spark_store}}' --to 'ssh://root@{{mono-gateway-host}}' "$P" && \
     ssh root@{{mono-gateway-host}} "nix-env --profile /nix/var/nix/profiles/system --set \"$P\"" && \
     just mono-gateway-activate "$P"

# Activate a closure without letting this ssh session be what keeps it alive.
#
# shortcut: the poll only checks the configured mono-gateway-host. A generation that
# moves the address will not answer there -- if this reports no marker or times out,
# the board is fine, you are just not looking at its new address. Upgrade: poll the
# address the new generation declares, or boot-menu over UART.
#
# switch-to-configuration runs as a child of the session that invokes it, so an
# activation that changes the network this session is riding in on kills itself
# halfway -- leaving a half-activated board and no way back in. That is exactly what
# the fold needed hand-held with setsid on 2026-10-17; this makes it the normal path
# for every deploy, and polls the board instead of trusting the session held.
mono-gateway-activate PROFILE:
    @ssh -n root@{{mono-gateway-host}} "rm -f /tmp/activate.exit /tmp/activate.log; setsid sh -c '{{PROFILE}}/bin/switch-to-configuration switch >/tmp/activate.log 2>&1; echo \$? >/tmp/activate.exit' </dev/null >/dev/null 2>&1 &"
    @for _ in $(seq 30); do sleep 4; ssh -o ConnectTimeout=5 -o ServerAliveInterval=15 root@{{mono-gateway-host}} 'test -f /tmp/activate.exit' 2>/dev/null && break; done
    @ssh -o ConnectTimeout=10 root@{{mono-gateway-host}} 'echo "running: $(readlink /run/current-system)"; echo "activation exit: $(cat /tmp/activate.exit 2>/dev/null || echo no-marker)"; echo "failed units: $(systemctl --failed --no-legend | wc -l)"; tail -2 /tmp/activate.log 2>/dev/null'

# Roll the board back to the previous generation and activate it. Works because the
# deploy target registers a profile. Note nixos-rebuild --rollback does NOT work here:
# it nix-builds its own wrapper from <nixpkgs>, and this board has no route. So this
# uses the primitives. Switching back twice is idempotent (it re-points at the older
# generation, it does not create a new one). Activates through the recipe above, since
# an old generation may well change the network.
mono-gateway-rollback:
    @P=$(ssh root@{{mono-gateway-host}} 'G=$(readlink /nix/var/nix/profiles/system | sed -E "s/.*system-([0-9]+)-link/\\1/"); nix-env --profile /nix/var/nix/profiles/system --switch-generation $((G>1 ? G-1 : 1)) 2>&1 | head -1; echo "from generation $G" >&2; readlink -f /nix/var/nix/profiles/system'); just mono-gateway-activate "$P"

# Roll forward to a generation by number without rebuilding -- the other half of a
# rehearsal. Also activates detached, so it survives its own network change.
mono-gateway-switch-generation NUM:
    @P=$(ssh root@{{mono-gateway-host}} 'nix-env --profile /nix/var/nix/profiles/system --switch-generation {{NUM}} 2>&1 | head -1; readlink -f /nix/var/nix/profiles/system'); just mono-gateway-activate "$P"

# Router management lives in openwrt/justfile, reachable as `just openwrt-...`.
import 'openwrt/justfile'
