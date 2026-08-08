{
  epkgs = epkgs: [ epkgs.embark-consult ];
  elisp = ''
    (use-package embark-consult
      :after (embark consult))
  '';
}
