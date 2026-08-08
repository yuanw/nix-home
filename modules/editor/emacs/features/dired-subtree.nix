{
  epkgs = epkgs: [
    epkgs.dired-subtree
  ];

  elisp = ''
    (use-package dired-subtree
      :after dired)
  '';
}
