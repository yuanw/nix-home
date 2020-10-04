{ config, lib, pkgs, ... }:

{
  imports = lib.attrValues (import ../../modules);
  networking.hostName = "yuan-mac";
}
