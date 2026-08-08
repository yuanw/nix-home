{
  epkgs = epkgs: [
    epkgs.guru-mode
  ];

  elisp = ''
    (use-package guru-mode
      :config
      (setq guru-warn-only t)
      (guru-global-mode 1))
  '';
}
