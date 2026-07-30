{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.nix-mode
  ];

  elisp = up.mkUsePackage "nix-mode" {
    mode = [ ''"\\.nix\\'"'' ];
  };
}
