{
  epkgs = epkgs: [ epkgs.aggressive-indent ];
  elisp = ''
    (use-package aggressive-indent
      :commands (aggressive-indent-mode global-aggressive-indent-mode))
  '';
}
