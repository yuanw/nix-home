{
  elisp = ''
    (use-package savehist
      :config
      (setq history-delete-duplicates t)
      (setq savehist-additional-variables
            '(file-name-history
              kmacro-ring
              compile-history
              compile-command))
      (setq savehist-autosave-interval 60)
      (setq savehist-ignored-variables
            '(load-history
              flyspell-auto-correct-ring
              org-roam-node-history
              magit-revision-history
              org-read-date-history
              query-replace-history
              yes-or-no-p-history
              kill-ring))
      (savehist-mode 1))
  '';
}
