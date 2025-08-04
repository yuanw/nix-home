{
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.agenix.nixosModules.default
    inputs.declarative-jellyfin.nixosModules.default
    ../../modules/isponsorblocktv.nix
    ../../modules/ha.nix
    ../../modules/caddy.nix
    ./configuration.nix
  ];
}
