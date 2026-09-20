{
  epkgs = epkgs: [ epkgs.multi-vterm ];
  elisp = ''
    (use-package multi-vterm
      :commands (multi-vterm multi-vterm-project))
  '';
}
