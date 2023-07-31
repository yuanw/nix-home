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
      devenv = inputs.devenv.packages.${prev.system}.devenv;
    })
    (import ./hs-land/overlay.nix)
    (import ./overlays)
  ];
in
{

  config = {
    flake = {
      nixos-flake.lib = rec {
        inherit specialArgsFor;
        mkSystemConfig =
          { system
          , modules
          , isDarwin ? inputs.nixpkgs.lib.hasSuffix "-darwin" system
          , isNixOS ? !isDarwin
          , ...
          }:
          (if isDarwin then
            inputs.darwin.lib.darwinSystem
          else
            inputs.nixpkgs.lib.nixosSystem) {
            inherit system;
            specialArgs = { inherit nix-colors isNixOS isDarwin astro-nvim; };
            modules = modules ++ [{ nixpkgs.overlays = overlays; } ./modules]
              ++ (if isDarwin then
              ([
                inputs.agenix.darwinModules.age
                inputs.home-manager.darwinModules.home-manager
                ./macintosh.nix
              ]) else
              ([
                ./nixos_system.nix
                inputs.hosts.nixosModule
                {
                  networking.stevenBlackHosts = {
                    enable = true;
                    blockFakenews = true;
                    blockGambling = true;
                    blockPorn = true;
                    blockSocial = false;
                  };
                }
                inputs.agenix.nixosModules.age
                inputs.home-manager.nixosModules.home-manager
              ]));

          };

      };
    };
  };
}
