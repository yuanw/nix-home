# direnv's `use flake` through iets (github:numtide/iets).
#
# A warm load reads iets's own tracing cache and never asks the daemon, and the
# watches cover every file the evaluation read, not only flake.nix and
# flake.lock: https://github.com/numtide/iets/blob/main/docs/how-to/use-with-direnv.md
#
# This is the home-manager twin of iets's nixosModules.default, which cannot be
# imported here: that module writes programs.direnv, and on this repo direnv is
# a home-manager program, not a nix-darwin one.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.iets.direnv;
  system = pkgs.stdenv.hostPlatform.system;
in
{
  options.iets.direnv.enable = lib.mkEnableOption "iets's use flake in direnv, in place of nix-direnv's";

  config = lib.mkIf cfg.enable {
    imports = [ inputs.iets.nixosModules.default ];
    programs.direnv.enable = true;
    home.packages = [ inputs.iets.packages.${system}.default ];

  };
}
