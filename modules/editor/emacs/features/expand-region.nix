{
  epkgs = epkgs: [
    epkgs.expand-region
  ];

  elisp = ''
    (use-package expand-region
      :defer t)
  '';
}
