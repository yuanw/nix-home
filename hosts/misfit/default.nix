{
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.agenix.nixosModules.default
    inputs.declarative-jellyfin.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    ../../modules/isponsorblocktv.nix
    ../../modules/caddy.nix
    ./configuration.nix
  ];
}
