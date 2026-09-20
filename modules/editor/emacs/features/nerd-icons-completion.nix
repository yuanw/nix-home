{
  epkgs = epkgs: [
    epkgs.nerd-icons-completion
  ];

  elisp = ''
    (use-package nerd-icons-completion
      :after marginalia
      :config
      (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
  '';
}
