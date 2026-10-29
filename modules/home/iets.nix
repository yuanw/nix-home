# direnv's `use flake` through iets (github:numtide/iets).
#
# A warm load reads iets's own tracing cache and never asks the daemon, and the
# watches cover every file the evaluation read, not only flake.nix and
# flake.lock: https://github.com/numtide/iets/blob/main/docs/how-to/use-with-direnv.md
#
# This is the home-manager twin of iets's nixosModules.default, which cannot be
# imported here: it is a NixOS module, so it declares `iets.direnv.enable` (the
# option this file declares) and writes `environment.systemPackages`,
# `nix.package` and `programs.direnv.direnvrcExtra` -- none of which exist in
# home-manager. Measured, not assumed. The .envrc still says `use flake`.
{
  pkgs,
  inputs,
  ...
}:
{

  home.packages = [ inputs.iets.packages.${pkgs.stdenv.hostPlatform.system}.default ];

  # The repository's own direnvrc, verbatim: it defines use_flake in terms of
  # `iets print-dev-env`, its --gc-root and its cache watches. direnv reads
  # lib/*.sh in name order, so this lands after hm-nix-direnv.sh and wins --
  # the same trick the nixos module plays with direnvrcExtra. nix-direnv stays
  # enabled for `use nix`.
  xdg.configFile."direnv/lib/zz-iets.sh".source = "${inputs.iets}/nix/direnvrc";
}
