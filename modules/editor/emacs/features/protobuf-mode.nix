{
  epkgs = epkgs: [
    epkgs.protobuf-mode
  ];

  elisp = ''
    (use-package protobuf-mode
      :mode "\\.proto\\'")
  '';
}
