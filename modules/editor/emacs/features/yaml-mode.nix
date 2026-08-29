{
  epkgs = epkgs: [
    epkgs.yaml-mode
  ];

  elisp = ''
    (use-package yaml-mode
      :mode ("\\.\\(e?ya?\\|ra\\)ml\\'"))
  '';
}
