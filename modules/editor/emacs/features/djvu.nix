{
  epkgs = epkgs: [ epkgs.djvu ];
  elisp = ''
    (use-package djvu
      :defer t)
  '';
}
