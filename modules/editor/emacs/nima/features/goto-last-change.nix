{
  epkgs = epkgs: [ epkgs.goto-last-change ];
  elisp = ''
    (use-package goto-last-change
      :bind (("C-x C-/" . goto-last-change)))
  '';
}
