{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.go;

  # See https://github.com/NixOS/nixpkgs/issues/509480
  # gopls started shipping `/bin/modernize` as a dependency, which collides
  # with the one from `gotools`. Since gopls bundles modernize functionality
  # internally, drop the standalone binary from gotools.
  gotoolsWithoutModernize = pkgs.symlinkJoin {
    name = "gotools-without-modernize";
    paths = [ pkgs.gotools ];
    postBuild = ''
      rm -f "$out/bin/modernize"
    '';
  };
in
{
  options.modules.dev.go = {
    enable = mkEnableOption "go";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        gotoolsWithoutModernize
        pkgs.golangci-lint
        pkgs.gopls
      ];
      programs = {
        go = {

          enable = true;
          env.GOPATH = [ "${config.my.homeDirectory}/go" ];
          telemetry.mode = "off";

        };

      };
    };
  };
}
