{
  epkgs = epkgs: [ epkgs.vterm ];
  elisp = ''
    (use-package vterm
      :commands (vterm))
  '';
}
