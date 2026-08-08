{
  epkgs = epkgs: [ epkgs.alert ];
  elisp = ''
    (use-package alert
      :commands (alert))
  '';
}
