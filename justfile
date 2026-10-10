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
    @nix-update -f ./packages/release.nix pi-coding-agent --src-only --version=branch --override-filename ./packages/emacs/pi-coding-agent.nix
    @nix-update -f ./packages/release.nix md-ts-mode --src-only --version=branch --override-filename ./packages/emacs/md-ts-mode.nix
    @nix-update -f ./packages/release.nix markdown-table-wrap --src-only --version=branch --override-filename ./packages/emacs/markdown-table-wrap.nix
    @nix-update -f ./packages/release.nix thrift-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ultra-scroll --src-only --version=branch
    @nix-update -f ./packages/release.nix pi-cursor-agent --src-only --override-filename ./packages/pi-extensions/pi-cursor-agent/default.nix --version-regex 'pi-cursor-agent@(.+)'
    # Vendored lock: bump version/src; also refresh npmDepsHash. If production deps change, regenerate packages/pi-extensions/pi-interactive-shell.package-lock.json first.
    @nix-update -f ./packages/release.nix pi-interactive-shell --override-filename ./packages/pi-extensions/pi-interactive-shell.nix --version-regex 'v(.*)'
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

# The DGX Spark box never has nix-home-private: its URL is a Mac-local path and the
# repo is not anonymously fetchable. The system needs none of it, so disable it there
# rather than pretend the box can resolve it (blank/ is carried by the rsync below).
spark_disable_private := "--override-input nix-home-private path:/etc/nixos/blank"

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


