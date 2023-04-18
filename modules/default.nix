{ config, lib, pkgs, isDarwin, isNixOS, ... }:

{
  imports = import ./modules.nix { inherit lib isDarwin isNixOS; };

}
