{
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.agenix.nixosModules.default
    ../../modules/isponsorblocktv.nix
    ../../modules/caddy.nix
    ./configuration.nix
  ];
}
