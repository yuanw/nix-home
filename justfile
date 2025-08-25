host := `hostname -s`

# build os
build:
    @nix build ".#$(hostname)"

update-all:
    @nix flake update

update INPUT:
    @nix flake update lock --update-input {{INPUT}}

nix-update:
    @nix-update -f ./packages/release.nix claude-code-ide --src-only --version=branch
    @nix-update -f ./packages/release.nix consult-omni --src-only --version=branch
    @nix-update -f ./packages/release.nix hurl-mode --src-only --version=branch
    @nix-update -f ./packages/release.nix ultra-scroll --src-only --version=branch

sys-diff:
    @nix store diff-closures /run/current-system ./result
