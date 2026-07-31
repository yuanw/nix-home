{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.rust-mode
  ];

  elisp = up.mkUsePackage "rust-mode" {
    mode = [ ''"\\.rs\\'"'' ];
  };
}
