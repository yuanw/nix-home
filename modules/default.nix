{ pkgs, lib, ... }:

{
  # what is going on
  imports = import ./modules.nix { inherit pkgs lib; };
}
