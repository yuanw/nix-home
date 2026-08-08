{
  epkgs = epkgs: [
    epkgs.go-mode
  ];

  elisp = ''
    (use-package go-mode)
  '';
}
