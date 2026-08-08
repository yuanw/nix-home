{
  epkgs = epkgs: [ epkgs.lin ];
  elisp = ''
    (use-package lin
      :commands (lin-global-mode lin-mode))
  '';
}
