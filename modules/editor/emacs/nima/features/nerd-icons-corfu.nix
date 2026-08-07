{
  epkgs = epkgs: [
    epkgs.nerd-icons-corfu
  ];

  elisp = ''
    (use-package nerd-icons-corfu
      :after corfu
      :config
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  '';
}
