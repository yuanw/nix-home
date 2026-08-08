{
  epkgs = epkgs: [ epkgs.spacious-padding ];
  elisp = ''
    (use-package spacious-padding
      :commands (spacious-padding-mode))
  '';
}
