{
  inputs,
  ...
}:

{
  imports = [
    inputs.dgx-spark.nixosModules.dgx-spark
    inputs.disko.nixosModules.disko
    ../../modules/ds4.nix
    ./disk-config.nix
    ./configuration.nix
  ];

  nixpkgs.overlays = [ (import ../../packages) ];
}
