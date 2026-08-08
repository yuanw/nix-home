{
  epkgs = epkgs: [
    epkgs.flycheck-eglot
  ];

  elisp = ''
    (use-package flycheck-eglot
      :after (flycheck eglot)
      :config
      (global-flycheck-eglot-mode 1))
  '';
}
