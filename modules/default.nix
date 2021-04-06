{ pkgs, lib, ... }:

{
  imports = import ./modules.nix { inherit pkgs lib; };
}
