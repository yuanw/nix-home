{
  epkgs = epkgs: [ epkgs.nov ];
  elisp = ''
    (use-package nov
      :mode (("\.epub\'" . nov-mode)))
  '';
}
