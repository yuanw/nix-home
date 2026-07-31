{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.dockerfile-mode
  ];

  elisp = up.mkUsePackage "dockerfile-mode" {
    mode = [ ''"Dockerfile\\'"'' ];
  };
}
