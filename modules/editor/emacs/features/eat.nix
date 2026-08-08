{
  epkgs = epkgs: [ epkgs.eat ];
  elisp = ''
    (use-package eat
      :commands (eat eat-eshell-mode eat-project))
  '';
}
