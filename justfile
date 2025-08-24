host := `hostname -s`

# build os
build:
   @nix build ".#$(hostname)"

update-all:
	@nix flake update

update INPUT:
    @nix flake update lock --update-input {{INPUT}}
