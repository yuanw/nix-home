{ lib, pkgs, ... }:

let
  packagePath = ../../../../packages/emacs;
in
{
  overlay = import ../overrides.nix {
    inherit pkgs lib packagePath;
    emacsGhostel.emacsOverrides = _self: _super: { };
  };

  epkgs = epkgs: [
    epkgs.hel
    epkgs.hel-leader
    epkgs.consult
    epkgs.embark-consult
    epkgs.which-key
  ];

  elispFile = ./hel.el;
}
