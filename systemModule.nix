{ inputs, config, lib, ... }:
let
  nix-colors = inputs.nix-colors;
  astro-nvim = inputs.astro-nvim;
in
{

  config = {
    flake = {
      nixos-flake.lib = {
        mkLinuxSystem = system: mod:
          let
            isNixOS = true;
            isDarwin = false;
          in
          inputs.nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = { inherit nix-colors isNixOS isDarwin astro-nvim; };
            modules = [ mod ];
          };

        mkMacosSystem = system: mod:
          let
            isNixOS = true;
            isDarwin = false;

          in
          inputs.darwin.lib.darwinSystem {
            inherit system;
            specialArgs = { inherit nix-colors isNixOS isDarwin astro-nvim; };
            modules = [ mod ];
          };
      };
    };
  };
}
