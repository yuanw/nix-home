Check the difference between current system and newly built configuration.

Steps:
1. Check if ./result exists from a recent build
2. If not, suggest building first with `just build` or `nix build .#<hostname>`
3. Run `nix-diff /run/current-system ./result` to show differences
4. Explain what packages/configurations will change
5. Ask if the user wants to apply the changes
