Build a specific host configuration.

Steps:
1. Ask the user which host to build (available hosts: asche, misfit, yuanw, ci, wk01174, mist)
2. Build the configuration using `nix build .#<hostname>`
3. Show the build result and any errors
4. If successful, offer to show the diff using `nix-diff /run/current-system ./result`
