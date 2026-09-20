{
  epkgs = epkgs: [ epkgs.ace-window ];
  elisp = ''
    (use-package ace-window
      :bind (("M-o" . ace-window)))
  '';
}
