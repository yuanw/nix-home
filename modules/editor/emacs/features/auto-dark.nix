{
  epkgs = epkgs: [ epkgs.auto-dark ];
  elisp = ''
    (use-package auto-dark
      :after ef-themes
      :init
      ;; `modus-themes-to-toggle' is not available unless modus-themes is
      ;; loaded.  We use Ef themes here, so configure Auto Dark explicitly:
      ;; first list is dark themes, second list is light themes.
      (setq auto-dark-themes '((ef-owl) (ef-spring)))
      :config
      (auto-dark-mode 1))
  '';
}
