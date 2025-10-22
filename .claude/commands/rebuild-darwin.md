Apply the nix-darwin configuration to the current macOS system.

Steps:
1. First, build the configuration with `just build` or `nix build .#<hostname>`
2. If build succeeds, ask if the user wants to see the diff first
3. If yes, run `nix-diff /run/current-system ./result`
4. Apply the configuration using `./result/sw/bin/darwin-rebuild switch --flake .`
5. If there are issues, mention rollback option: `darwin-rebuild --rollback`
6. Verify the switch was successful

Note: This command is for macOS (darwin) hosts only.
