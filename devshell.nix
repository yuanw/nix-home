{ inputs, ... }:
{
  perSystem =
    {
      config,
      inputs',
      pkgs,
      ...
    }:
    let
      # Named Workiva tools from the flake-less private tree.  Forced only when a
      # wk package or the wk01174 shell is built; public hosts (and the blank
      # override) never touch this import.
      wkTools = import (inputs.nix-home-private + "/modules/devshell.nix") {
        inherit pkgs;
        privateSrc = inputs.nix-home-private;
      };
    in
    {
      # `nix run .#prefetch-work-sources` etc. — the justfile names these only.
      packages = {
        inherit (wkTools)
          prefetch-work-sources
          bump-semver-git-sources
          nvfetcher-work-sources
          pin-store-gc-root
          nix-build-with-workiva-netrc
          ;
      };

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          inputs'.colmena.packages.colmena
          nix-diff
          nix-tree
          dive
          treefmt
          lego
          just
          git
        ];
        inputsFrom = [
          config.treefmt.build.devShell
          config.pre-commit.devShell
        ];
      };

      # Shared shell plus the Workiva tools.  Keyed by host, not system: WK01174
      # and mist share aarch64-darwin and share devShells.default; the name is
      # what makes this the host's shell (`nix develop .#wk01174`).
      devShells.wk01174 = pkgs.mkShell {
        name = "wk01174";
        buildInputs = builtins.attrValues wkTools;
        inputsFrom = [ config.devShells.default ];
      };
    };
}
