{
  epkgs = epkgs: [
    epkgs.jinx
  ];

  elisp = ''
    (use-package jinx
      :diminish jinx-mode
      :commands (jinx-mode)
      :hook (org-mode . jinx-mode)
      :config
      (setq jinx-languages "en_CA"))
  '';
}
