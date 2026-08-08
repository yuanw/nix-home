{
  elisp = ''
    (use-package dired-x
      :hook (dired-mode . dired-omit-mode)
      :config
      (setq dired-omit-verbose nil)
      (keymap-set dired-mode-map "." #'dired-omit-mode)
      (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))
  '';
}
