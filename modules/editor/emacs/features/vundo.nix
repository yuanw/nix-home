{
  epkgs = epkgs: [ epkgs.vundo ];
  elisp = ''
    (use-package vundo
      :commands (vundo))
  '';
}
