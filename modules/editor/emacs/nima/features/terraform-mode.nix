{
  epkgs = epkgs: [
    epkgs.terraform-mode
  ];

  elisp = ''
    (use-package terraform-mode
      :mode ("\\.tf\\(vars\\)?\\'"))
  '';
}
