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
    @nix-update -f ./packages/release.nix claude-mem --src-only --version=branch --override-filename ./packages/claude-plugins/claude-mem.nix
    @nix-update -f ./packages/release.nix consult-omni --src-only --version=branch
    @nix-update -f ./packages/release.nix cozempic --src-only
    @nix-update -f ./packages/release.nix emacs-skills --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel-agent --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel-quick --src-only --version=branch
    @nix-update -f ./packages/release.nix humanizer --src-only --version=branch
    @nix-update -f ./packages/release.nix hurl-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix knockknock --src-only --version=branch
    @nix-update -f ./packages/release.nix lean4-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix magit-ai --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-racket --src-only --version=branch
    @nix-update -f ./packages/release.nix shell-maker --src-only --version=branch
    @nix-update -f ./packages/release.nix thrift-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ultra-scroll --src-only --version=branch
    @nix-update -f ./packages/release.nix vibeproxy --src-only
    @nix-update -f ./packages/release.nix pi-cursor-agent --src-only --override-filename ./packages/pi-extensions/pi-cursor-agent/default.nix --version-regex 'pi-cursor-agent@(.+)'
    @nix-update -f ./packages/release.nix tccutil --src-only



# apply nix configuration (works on both macOS and NixOS)
switch:
    @if [ "$(uname)" = "Darwin" ]; then \
        sudo ./result/sw/bin/darwin-rebuild switch --flake .; \
    else \
        nixos-rebuild switch --flake '.#' --quiet --sudo; \
    fi

sys-diff:
    @nix store diff-closures /run/current-system ./result

# clear easysession saved sessions (fixes stale dired/buffer paths after Emacs rebuild)
clean-easysession:
    @rm -rf ~/.emacs.d/easysession/
    @echo "easysession cleared"

# toggle .envrc file
toggle-envrc:
    @if [ -f .envrc ]; then mv .envrc .envrc.bk && echo "Moved .envrc to .envrc.bk"; elif [ -f .envrc.bk ]; then mv .envrc.bk .envrc && echo "Moved .envrc.bk to .envrc"; else echo "Neither .envrc nor .envrc.bk exists"; fi
