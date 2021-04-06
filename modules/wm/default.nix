{ config, lib, pkgs, ... }:

let



lib.mkMerge [

  (lib.mkIf pkgs.stdenv.isDarwin { imports = [ ./yabai.nix ]; })

]
