{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.yaml-mode
  ];

  elisp = up.mkUsePackage "yaml-mode" {
    mode = [ ''"\\.\\(e?ya?\\|ra\\)ml\\'"'' ];
  };
}
