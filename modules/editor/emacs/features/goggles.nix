{
  epkgs = epkgs: [ epkgs.goggles ];
  elisp = ''
    (use-package goggles
      :hook ((prog-mode text-mode) . goggles-mode)
      :config
      (setq goggles-pulse t))
  '';
}
