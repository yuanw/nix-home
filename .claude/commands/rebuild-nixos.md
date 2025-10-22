Apply the NixOS configuration to the current Linux system.

Steps:
1. First, build the configuration for the host
2. If build succeeds, ask if the user wants to see the diff first
3. If yes, run `nix-diff /run/current-system ./result`
4. Apply the configuration using `nixos-rebuild switch --flake '.#' --use-remote-sudo`
5. If there are issues, mention rollback options
6. Verify the switch was successful

Note: This command is for NixOS (Linux) hosts only (asche, misfit).
