{
  epkgs = epkgs: [ epkgs.editorconfig ];
  elisp = ''
    (use-package editorconfig
      :config
      (editorconfig-mode 1))
  '';
}
