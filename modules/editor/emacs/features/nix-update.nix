{
  epkgs = epkgs: [
    epkgs.nix-update
  ];

  elisp = ''
    (use-package nix-update
      :defer t)
  '';
}
