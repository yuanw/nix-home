{ lib, pkgs, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.deadgrep
    pkgs.ripgrep
  ];

  elisp = up.mkUsePackage "deadgrep" {
    config = ''
      (setq deadgrep-executable "${pkgs.ripgrep}/bin/rg")
    '';
  };
}
