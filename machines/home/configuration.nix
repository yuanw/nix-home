{ config, lib, pkgs, ... }:

{
 imports = lib.attrValues (import ../../modules);
}
