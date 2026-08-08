{
  epkgs = epkgs: [ epkgs.posframe ];
  elisp = ''
    (use-package posframe
      :defer t)
  '';
}
