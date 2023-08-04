{
  perSystem = { config, pkgs, ... }: {
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        nix-diff
        awscli
        lego
        terraform
      ];
      # See https://haskell.flake.page/devshell#composing-devshells
      inputsFrom = [
        config.treefmt.build.devShell
        config.pre-commit.devShell
      ];
    };
  };
}
