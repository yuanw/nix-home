{
  perSystem =
    { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          nix-diff
          nix-tree
          dive
          treefmt
          #awscli
          lego
          #terraform
        ];
        # See https://haskell.flake.page/devshell#composing-devshells
        inputsFrom = [
          config.treefmt.build.devShell
          config.pre-commit.devShell
          # config.haskellProjects.default.outputs.devShell

        ];
      };
      # devShells.haskell = pkgs.mkShell {
      #   inputsFrom = [
      #     config.treefmt.build.devShell
      #     config.pre-commit.devShell
      #     #config.haskellProjects.default.outputs.devShell
      #   ];
      # };

    };
}
