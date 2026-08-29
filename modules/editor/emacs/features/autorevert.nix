{
  elisp = ''
    (use-package autorevert
      :hook ((after-init . global-auto-revert-mode)
             (dired-mode . auto-revert-mode))
      :config
      (setq auto-revert-use-notify nil)
      (setq auto-revert-verbose t))
  '';
}
