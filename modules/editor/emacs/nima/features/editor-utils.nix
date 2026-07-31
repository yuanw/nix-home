{ ... }:

{
  epkgs = epkgs: [
    epkgs.gcmh
    epkgs.browse-kill-ring
    epkgs.emacs-everywhere
    epkgs.expand-region
  ];
  elispFile = ./editor-utils.el;
}
