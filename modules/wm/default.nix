{
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkMerge [
    (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin { imports = [ ./yabai.nix ]; })

  ];
}
