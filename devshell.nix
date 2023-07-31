{
  perSystem = { config, pkgs, ... }: {
    devShells.default = pkgs.mkShell {

      # See https://haskell.flake.page/devshell#composing-devshells
      inputsFrom = [
        config.treefmt.build.devShell
      ];
    };
  };
}
