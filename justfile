host := `hostname -s`

# build os
build:
    @nix build ".#$(hostname)"

update-all:
    @nix flake update

update INPUT:
    @nix flake update lock --update-input {{INPUT}}

# update ad-hoc packages upstream references 
nix-update:
    @nix-update -f ./packages/release.nix auto-save --src-only --version=branch
    @nix-update -f ./packages/release.nix claude-code-ide --src-only --version=branch
    @nix-update -f ./packages/release.nix consult-omni --src-only --version=branch
    @nix-update -f ./packages/release.nix emacs-reader --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix gptel-quick --src-only --version=branch
    @nix-update -f ./packages/release.nix lean4-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix hurl-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-gptel --src-only --version=branch
    @nix-update -f ./packages/release.nix ob-racket --src-only --version=branch
    @nix-update -f ./packages/release.nix thrift-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ultra-scroll --src-only --version=branch
    @nix-update -f ./packages/release.nix home-row-expreg --src-only --version=branch

sys-diff:
    @nix store diff-closures /run/current-system ./result

# toggle .envrc file
toggle-envrc:
    @if [ -f .envrc ]; then mv .envrc .envrc.bk && echo "Moved .envrc to .envrc.bk"; elif [ -f .envrc.bk ]; then mv .envrc.bk .envrc && echo "Moved .envrc.bk to .envrc"; else echo "Neither .envrc nor .envrc.bk exists"; fi
