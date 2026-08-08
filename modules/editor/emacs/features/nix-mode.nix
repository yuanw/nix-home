{
  epkgs = epkgs: [
    epkgs.nix-mode
  ];

  elisp = ''
    (use-package nix-mode
      :mode ("\\.nix\\'"))
  '';
}
