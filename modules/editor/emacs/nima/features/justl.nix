{
  epkgs = epkgs: [ epkgs.justl ];
  elisp = ''
    (use-package justl
      :commands (justl justl-exec-recipe))
  '';
}
