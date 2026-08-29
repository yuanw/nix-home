{
  epkgs = epkgs: [
    epkgs.consult-project-extra
  ];

  elisp = ''
    (use-package consult-project-extra
      :bind (("C-c p f" . consult-project-extra-find)
             ("C-c p o" . consult-project-extra-find-other-window)))
  '';
}
