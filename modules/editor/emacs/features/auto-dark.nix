{
  epkgs = epkgs: [ epkgs.auto-dark ];
  elisp = ''
    (use-package auto-dark
      :commands (auto-dark-mode))
  '';
}
