Update flake inputs for this Nix configuration.

Steps:
1. Ask the user if they want to update all inputs or a specific input
2. If all: run `just update-all` or `nix flake update`
3. If specific: ask which input and run `just update <INPUT>` or `nix flake lock --update-input <INPUT>`
4. Show what was updated
5. Suggest building to verify the updates work
