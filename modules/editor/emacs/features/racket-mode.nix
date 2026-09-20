{
  epkgs = epkgs: [
    epkgs.racket-mode
  ];

  elisp = ''
    (use-package racket-mode)
  '';
}
