{ config, lib, pkgs, ... }:

{

  config = lib.mkMerge [

    (lib.mkIf pkgs.stdenv.isDarwin { imports = [ ./yabai.nix ]; })

  ];
}
