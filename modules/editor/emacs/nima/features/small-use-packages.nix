{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
up.mkUsePackageFeatures {
  # Example of the Home Manager-like style we want for simple packages.  The
  # package defaults to the attribute name.
  disable-mouse = {
    enable = false;
    config = ''
      (global-disable-mouse-mode)
    '';
  };
}
