{
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    ../../modules/isponsorblocktv.nix
    ../../modules/caddy.nix

  ];
}
