{
  epkgs = epkgs: [
    epkgs.consult-denote
  ];

  elisp = ''
    (use-package consult-denote
      :after consult
      :bind (("C-c n f" . consult-denote-find)
             ("C-c n r" . consult-denote-grep))
      :config
      (setq consult-denote-find-command 'consult-fd)
      (consult-denote-mode 1))
  '';
}
