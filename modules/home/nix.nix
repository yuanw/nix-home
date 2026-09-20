# User-level Nix package selection only.
# Substituters and trusted keys must live in daemon/system config; putting them
# in Home Manager's user nix.conf can override cache.nixos.org and is ignored
# for untrusted users.
{ lib, pkgs, ... }:
{
  nix.package = lib.mkDefault pkgs.nix;
}
