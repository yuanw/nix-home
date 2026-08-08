{
  elisp = ''
    (use-package recentf
      :hook (after-init . recentf-mode)
      :commands (recentf-mode recentf-add-file recentf-apply-filename-handlers)
      :config
      (setq recentf-auto-cleanup nil)
      (setq recentf-max-saved-items 100)
      (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG")))
  '';
}
