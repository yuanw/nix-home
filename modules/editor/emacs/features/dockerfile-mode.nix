{
  epkgs = epkgs: [
    epkgs.dockerfile-mode
  ];

  elisp = ''
    (use-package dockerfile-mode
      :mode ("Dockerfile\\'"))
  '';
}
