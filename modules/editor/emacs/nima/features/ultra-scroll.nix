{ lib, pkgs, ... }:

let
  packagePath = ../../../../../packages/emacs;
in
{
  overlay = import ../../overrides.nix {
    inherit pkgs lib packagePath;
    emacsGhostel.emacsOverrides = _self: _super: { };
  };

  epkgs = epkgs: [ epkgs.ultra-scroll ];

  elisp = ''
    (use-package ultra-scroll
      :hook (after-init . ultra-scroll-mode)
      :custom
      (scroll-conservatively 101)
      (scroll-margin 0))
  '';
}
