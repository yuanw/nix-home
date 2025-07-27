{
  self,
}:
{
  imports = [
    self.inputs.disko.nixosModules.disko
    ./configuration.nix
  ];
}
