{ config, pkgs, ... }:

{
  imports = [ ./dart.nix ./editor.nix ./hosts.nix ./workShell.nix ];
}
