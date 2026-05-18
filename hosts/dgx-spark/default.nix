{
  inputs,
  ...
}:

{
  imports = [
    inputs.dgx-spark.nixosModules.dgx-spark
    inputs.disko.nixosModules.disko
    ./disk-config.nix
    ./configuration.nix
  ];

  nixpkgs.overlays = [ (import ../../packages) ];
}
