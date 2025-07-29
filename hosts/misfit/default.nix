{
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    ./isponsorblocktv.nix
    ./configuration.nix
  ];
}
