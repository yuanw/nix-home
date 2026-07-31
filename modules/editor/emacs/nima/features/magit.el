;;; magit.el --- Magit configuration -*- lexical-binding: t; -*-

(use-package magit
  :demand t
  :commands (magit-project-status)
  :bind (("C-x g" . magit-status)
         :map magit-mode-map
         ("U" . magit-unstage-all)
         ("k" . magit-discard))
  :config
  (setq magit-list-refs-sortby "-committerdate")
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package forge
  :after magit
  :config
  (setq forge-add-pullreq-refspec 'ask)
  (when-let ((gh (executable-find "gh")))
    (let ((token (string-trim (shell-command-to-string (format "%s auth token" gh)))))
      (when (and token (not (string-empty-p token)))
        (setenv "GH_TOKEN" token)))))

(provide 'nima-feature-magit)
;;; magit.el ends here
