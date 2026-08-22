{
  epkgs = epkgs: [ epkgs.auto-dark ];
  elisp = ''


    (use-package auto-dark
      :custom
      (auto-dark-themes (mapcar #'list modus-themes-to-toggle))
      :init
      (auto-dark-mode))
  '';
}
