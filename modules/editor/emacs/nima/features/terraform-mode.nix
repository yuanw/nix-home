{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.terraform-mode
  ];

  elisp = up.mkUsePackage "terraform-mode" {
    mode = [ ''"\\.tf\\(vars\\)?\\'"'' ];
  };
}
