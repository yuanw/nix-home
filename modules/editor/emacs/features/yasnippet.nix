{
  epkgs = epkgs: [ epkgs.yasnippet ];
  elisp = ''
    (use-package yasnippet
      :config
      (yas-global-mode 1))
  '';
}
