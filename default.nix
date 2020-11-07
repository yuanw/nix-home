{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
, pkgs ? import sources.nixpkgs { inherit system; }
, configuration ? ./machines/yuan-mac/configuration.nix
, darwin ? import sources.nix-darwin { inherit nixpkgs configuration system pkgs; }
}: {
  nix-darwin = darwin.system;
}
