{
  perSystem = { config, pkgs, ... }: {
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        (python3.withPackages (ps: [ ps.invoke ]))
        nix-diff
        nix-tree
        awscli
        lego
        terraform
      ];
      # See https://haskell.flake.page/devshell#composing-devshells
      inputsFrom = [
        config.treefmt.build.devShell
        config.pre-commit.devShell
        config.haskellProjectsdefault.outputs.devShell

      ];
    };
    devShells.haskell = pkgs.mkShell {
      inputsFrom = [
        config.treefmt.build.devShell
        config.pre-commit.devShell
        config.haskellProjectsdefault.outputs.devShell
      ];
    };

  };
}
