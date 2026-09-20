{
  epkgs = epkgs: [
    epkgs.nerd-icons
  ];

  elisp = ''
    (use-package nerd-icons
      :defer t
      :config
      (setq nerd-icons-font-family "PragmataPro Mono Liga"))
  '';
}
