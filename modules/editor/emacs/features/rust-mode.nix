{
  epkgs = epkgs: [
    epkgs.rust-mode
  ];

  elisp = ''
    (use-package rust-mode
      :mode ("\\.rs\\'"))
  '';
}
