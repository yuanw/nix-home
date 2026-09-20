{
  epkgs = epkgs: [
    epkgs.free-keys
  ];

  elisp = ''
    (use-package free-keys
      :defer 1
      :commands (free-keys))
  '';
}
