{
  perSystem =
    {
      config,
      inputs',
      pkgs,
      system,
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
        ];
        inputsFrom = [
          config.treefmt.build.devShell
          config.pre-commit.devShell
        ];
      };

      # ComfyUI devshell — aarch64-linux only (DGX Spark)
      # Uses the comfyui package from our overlay (nixified-ai + CUDA 13.2)
      devShells.comfyui = pkgs.mkShell {
        packages = pkgs.lib.optionals (system == "aarch64-linux") [
          pkgs.comfyui
        ];
        shellHook = pkgs.lib.optionalString (system == "aarch64-linux") ''
          echo "ComfyUI — run: comfyui --listen 0.0.0.0"
        '';
      };
    };
}
