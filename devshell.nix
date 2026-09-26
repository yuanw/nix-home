top@{ inputs, ... }:
{  perSystem =
    {
      config,
      inputs',
      pkgs,
      ...
    }:
    {
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

        # The WK01174 dev shell: the shared one, plus the Workiva maintenance
        # tools, which live over in nix-home-private.  It is keyed by host and not
        # by system, because WK01174 and mist are the same system
        # (aarch64-darwin) and share devShells.default; the name is what makes it
        # the host'&#39;s shell, and nix develop .#wk01174 is how you get into it.
        #
        # The tools arrive as packages built by the private repository, so that
        # the justfile names the shell rather than their paths -- see
        # nix-home-private/modules/devshell.nix for why they are a function and not
        # a flake-parts module.  Nothing of the shared shell is repeated here:
        # it is taken as an input, which is the nix analogue of linking.
        devShells.wk01174 = pkgs.mkShell {
          name = "wk01174";
          buildInputs = import (inputs.nix-home-private + "/modules/devshell.nix") {
            inherit pkgs;
            privateSrc = inputs.nix-home-private;
          };
          inputsFrom = [ config.devShells.default ];
        };
    };
}
