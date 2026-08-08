{
  epkgs = epkgs: [
    epkgs.geiser
  ];

  elisp = ''
    (use-package geiser
      :defer t)
  '';
}
