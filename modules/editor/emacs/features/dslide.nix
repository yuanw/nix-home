{
  epkgs = epkgs: [ epkgs.dslide ];
  elisp = ''
    (use-package dslide)
  '';

}
