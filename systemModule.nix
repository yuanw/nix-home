{ self, inputs, config, lib, ... }:
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
  nix-colors = inputs.nix-colors;
  astro-nvim = inputs.astro-nvim;
  overlays = [
    inputs.emacs.overlay
    inputs.nur.overlay
    inputs.agenix.overlays.default
    (_final: prev: {
      stable = inputs.nixpkgs-stable.legacyPackages.${prev.system};
      mesa = inputs.nixpkgs-stable.legacyPackages.${prev.system}.mesa;
      # use this variant if unfree packages are needed:
      # unstable = import nixpkgs-unstable {
      #   inherit system;
      #   config.allowUnfree = true;
      # };

    })
    (_final: prev: {
      reiryoku-firmware = inputs.reiryoku.packages.${prev.system}.firmware;
      # devenv = inputs.devenv.packages.${prev.system}.devenv;
    })
    (import ./hs-land/overlay.nix)
    (import ./packages)
  ];
in
{

  config = {
    flake = {
      nixos-flake.lib =  {
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
          inputs.nix-darwin.lib.darwinSystem {
          inherit system;
            specialArgs = { inherit nix-colors isNixOS isDarwin astro-nvim; };
          modules = [ mod ];
        };
      };
    };
  };
}
