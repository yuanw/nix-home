{
  inputs,
  ...
}:

{
  imports = [
    inputs.dgx-spark.nixosModules.dgx-spark
    inputs.disko.nixosModules.disko
    ../../modules/ds4.nix
    ../../modules/lance.nix
    ./disk-config.nix
    ./configuration.nix
  ];

  nixpkgs.overlays = [ (import ../../packages) ];
}
