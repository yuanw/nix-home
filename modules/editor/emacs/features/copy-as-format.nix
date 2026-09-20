{
  epkgs = epkgs: [ epkgs.copy-as-format ];
  elisp = ''
    (use-package copy-as-format
      :commands (copy-as-format copy-as-format-slack copy-as-format-github))
  '';
}
