{ self, inputs, config, flake-parts-lib, lib, ... }:
let
  specialArgsFor = rec {
    common = {
      flake = { inherit self inputs config; };
    };
    nixos = common;
    darwin = common // {
      rosettaPkgs = import inputs.nixpkgs { system = "x86_64-darwin"; };
    };
  };
in
{

  config = {
    flake = {
      nixos-flake.lib = rec {
        inherit specialArgsFor;

        mkLinuxSystem = system: mod: inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          # Arguments to pass to all modules.
          specialArgs = specialArgsFor.nixos;
          modules = [ mod ];
        };

        mkMacosSystem = system: mod: inputs.nix-darwin.lib.darwinSystem {
          inherit system;
          specialArgs = specialArgsFor.darwin;
          modules = [
            inputs.agenix.darwinModules.age
            inputs.home-manager.darwinModules.home-manager
            mod
          ];
        };

      };
    };
  };
}
