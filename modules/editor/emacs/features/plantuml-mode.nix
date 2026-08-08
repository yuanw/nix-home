{
  epkgs = epkgs: [
    epkgs.plantuml-mode
  ];

  elisp = ''
    (use-package plantuml-mode)
  '';
}
