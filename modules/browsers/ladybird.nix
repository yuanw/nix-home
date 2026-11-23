# macOS: Ladybird from upstream's notarized arm64 disk image.
# nixpkgs also builds `ladybird' from source, but it compiles skia, Qt and the
# Rust pieces; this is the vendor bundle (packages/ladybird/package.nix).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.browsers.ladybird;
in
{
  options.modules.browsers.ladybird = {
    enable = lib.mkEnableOption "Ladybird from the upstream macOS disk image";
    pkg = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ladybird-bin;
      description = ''
        Ladybird package.  Home Manager links its Ladybird.app into
        ~/Applications/Home Manager Apps, which is what modules.browsers
        darwinOpenTarget launches.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username}.home.packages = [ cfg.pkg ];
  };
}
