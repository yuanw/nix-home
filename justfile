host := `hostname -s`

# list all commands
default:
    @just --list

# build os
build:
    @nix build --quiet ".#{{lowercase(host)}}"

update-all:
    @nix flake update

update INPUT:
    @nix flake update lock --update-input {{INPUT}}

# update ad-hoc packages upstream references 
nix-update:
    @nix-update -f ./packages/release.nix agent-shell --src-only --version=branch
    @nix-update -f ./packages/release.nix auto-save --src-only --version=branch
    @nix-update -f ./packages/release.nix caveman --src-only --version=branch
    @nix-update -f ./packages/release.nix chroma-mcp --src-only
    @nix-update -f ./packages/release.nix cursor-agent-acp --src-only
    @nix-update -f ./packages/release.nix pi-acp --src-only --version=branch --override-filename ./packages/pi-acp.nix
    @nix-update -f ./packages/release.nix consult-omni --src-only --version=branch
    @nix-update -f ./packages/release.nix emacs-skills --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel-agent --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel-quick --src-only --version=branch
    @nix-update -f ./packages/release.nix humanizer --src-only --version=branch
    @nix-update -f ./packages/release.nix hurl-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix knockknock --src-only --version=branch
    @nix-update -f ./packages/release.nix lean4-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-racket --src-only --version=branch
    @nix-update -f ./packages/release.nix shell-maker --src-only --version=branch --override-filename ./packages/emacs/shell-maker.nix
    @nix-update -f ./packages/release.nix thrift-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ultra-scroll --src-only --version=branch
    @nix-update -f ./packages/release.nix vibeproxy --src-only
    @nix-update -f ./packages/release.nix pi-cursor-agent --src-only --override-filename ./packages/pi-extensions/pi-cursor-agent/default.nix --version-regex 'pi-cursor-agent@(.+)'
    @nix-update -f ./packages/release.nix tccutil --src-only
    @nix-update -f ./packages/release.nix ds4 --src-only --version=branch

update-wk:
	nvfetcher -c modules/private/nvfetcher.toml -o modules/private/_sources

# deploy to DGX Spark: sync flake and rebuild remotely
spark-deploy IP="dgx-spark.local":
    @rsync -av --exclude=.git --exclude=result ./ "yuanw@{{IP}}:/etc/nixos/"
    @ssh yuanw@{{IP}} "cd /etc/nixos && sudo nixos-rebuild switch --flake .#dgx-spark"

# DS4 server management on DGX Spark
spark-ds4-start IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl start ds4-server"

spark-ds4-stop IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl stop ds4-server"

spark-ds4-restart IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl restart ds4-server"

spark-ds4-status IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl status ds4-server"

spark-ds4-logs IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo journalctl -u ds4-server -f"

spark-ds4-download MODEL="q2-imatrix" IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo -u ds4 bash -c 'cd /var/lib/ds4 && ds4-download-model {{MODEL}}'"

# ─── Lance AI Server ────────────────────────────────────────────────

# deploy lance changes to DGX Spark and rebuild
spark-lance-deploy IP="dgx-spark.local":
    @rsync -av --exclude=.git --exclude=result ./ "yuanw@{{IP}}:/etc/nixos/"
    @ssh yuanw@{{IP}} "cd /etc/nixos && sudo nixos-rebuild switch --flake .#dgx-spark"

# download both Lance model variants (Lance_3B + Lance_3B_Video) on DGX Spark
spark-lance-download-models IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo -u lance bash -c 'export LANCE_DATA_DIR=/var/lib/lance && \$(command -v lance-download-model)'"

# service management (replace INSTANCE with "video" or "image")
spark-lance-start INSTANCE="video" IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl start lance-gradio-{{INSTANCE}}"

spark-lance-stop INSTANCE="video" IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl kill --kill-who=main --signal=SIGKILL lance-gradio-{{INSTANCE}} 2>/dev/null; sudo systemctl stop lance-gradio-{{INSTANCE}}"

# start both image & video services (stops ds4-server first)
spark-lance-start-both IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl stop ds4-server; sudo systemctl start lance-gradio-image lance-gradio-video"

# stop both image & video services (force kill first)
spark-lance-stop-both IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl kill --kill-who=main --signal=SIGKILL lance-gradio-image lance-gradio-video 2>/dev/null; sudo systemctl stop lance-gradio-image lance-gradio-video"

# restart both services
spark-lance-restart-both IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl stop ds4-server; sudo systemctl restart lance-gradio-image lance-gradio-video"

spark-lance-restart INSTANCE="video" IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl restart lance-gradio-{{INSTANCE}}"

spark-lance-status INSTANCE="video" IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo systemctl status lance-gradio-{{INSTANCE}}"

spark-lance-logs INSTANCE="video" IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "sudo journalctl -u lance-gradio-{{INSTANCE}} -f"

spark-lance-check-models IP="dgx-spark.local":
    @ssh yuanw@{{IP}} "ls -lh /var/lib/lance/downloads/"

# build lance on DGX Spark (without switching)
spark-build-lance IP="dgx-spark.local":
    @rsync -av --exclude=.git --exclude=result ./ "yuanw@{{IP}}:/etc/nixos/"
    @ssh yuanw@{{IP}} "cd /etc/nixos && nixos-rebuild build --flake .#dgx-spark"

# build ds4 on DGX Spark (without switching)
spark-build-ds4 IP="dgx-spark.local":
    @rsync -av --exclude=.git --exclude=result ./ "yuanw@{{IP}}:/etc/nixos/"
    @ssh yuanw@{{IP}} "cd /etc/nixos && nixos-rebuild build --flake .#dgx-spark"

# unlock SSH key (Linux: GUI passphrase prompt, macOS: uses Keychain)
_unlock-ssh:
    @if [ "$(uname)" = "Darwin" ]; then \
        ssh-add -l 2>/dev/null || ssh-add --apple-use-keychain ~/.ssh/id_ed25519; \
    else \
        eval `ssh-agent -s` && setsid ssh-add ~/.ssh/id_ed25519 < /dev/null; \
    fi

# build on DGX Spark using colmena
colmena-spark-build: _unlock-ssh
    colmena build --on dgx-spark

# apply (build + switch) on DGX Spark using colmena
colmena-spark-apply: _unlock-ssh
    colmena apply --on dgx-spark

# build and deploy to local host (macOS or NixOS)
switch:
    @if [ "$(uname)" = "Darwin" ]; then \
        sudo darwin-rebuild switch --flake .; \
    else \
        nixos-rebuild switch --flake '.#' --quiet --sudo; \
    fi

sys-diff:
    @nix store diff-closures /run/current-system ./result


