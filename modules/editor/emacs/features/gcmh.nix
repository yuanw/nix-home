{
  epkgs = epkgs: [
    epkgs.gcmh
  ];

  elisp = ''
    (use-package gcmh
      :defer 1
      :commands (gcmh-mode)
      :diminish gcmh-mode
      :config
      (setq gcmh-idle-delay 'auto)
      (gcmh-mode 1))
  '';
}
