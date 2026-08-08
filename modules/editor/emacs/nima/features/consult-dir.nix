{
  epkgs = epkgs: [ epkgs.consult-dir ];
  elisp = ''
    (use-package consult-dir
      :after consult
      :commands (consult-dir))
  '';
}
