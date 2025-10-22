Format all Nix files and other code in the repository.

Steps:
1. Run `nix fmt` to format all files using treefmt
2. Show what files were formatted
3. Note: This runs deadnix, nixfmt, ormolu, cabal-fmt, hlint, shellcheck, and shfmt
4. If there are errors, show them and suggest fixes
