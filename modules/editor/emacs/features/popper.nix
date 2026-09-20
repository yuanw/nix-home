{
  epkgs = epkgs: [ epkgs.popper ];
  elisp = ''
    (use-package popper
      :bind (("C-`" . popper-toggle)
             ("M-`" . popper-cycle)
             ("C-M-`" . popper-toggle-type))
      :config
      (popper-mode 1)
      (popper-echo-mode 1))
  '';
}
