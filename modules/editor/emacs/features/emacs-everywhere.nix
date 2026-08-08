{
  epkgs = epkgs: [
    epkgs.emacs-everywhere
  ];

  elisp = ''
    (use-package emacs-everywhere
      :config
      (setq emacs-everywhere--dir
            (locate-user-emacs-file "everywhere")))
  '';
}
