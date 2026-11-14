{
  epkgs = epkgs: [ epkgs.lin ];
  elisp = ''
    (use-package lin
      :hook (after-init . lin-global-mode)
      :config
      (setq lin-face 'lin-green))
  '';
}
