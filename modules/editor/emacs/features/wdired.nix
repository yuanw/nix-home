{
  elisp = ''
    (use-package wdired
      :after dired
      :config
      (setq wdired-allow-to-change-permissions t)
      (keymap-set dired-mode-map "C-c C-w" #'wdired-change-to-wdired-mode))
  '';
}
