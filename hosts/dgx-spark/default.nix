{
  inputs,
  ...
}:

{
  imports = [
    inputs.dgx-spark.nixosModules.dgx-spark
    inputs.preservation.nixosModules.preservation
    inputs.disko.nixosModules.disko
    ./disk-config.nix
    ./configuration.nix
  ];
}
