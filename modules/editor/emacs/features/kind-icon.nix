{
  epkgs = epkgs: [ epkgs.kind-icon ];
  elisp = ''
    (use-package kind-icon
      :after corfu)
  '';
}
