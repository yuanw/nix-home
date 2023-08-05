{
  perSystem = { config, pkgs, ... }: {
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        nix-diff
        nix-tree
        awscli
        lego
        terraform
        wifish
      ];
      # See https://haskell.flake.page/devshell#composing-devshells
      inputsFrom = [
        config.treefmt.build.devShell
        config.pre-commit.devShell
      ];
    };
  };
}
