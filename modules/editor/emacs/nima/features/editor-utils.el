;;; editor-utils.el --- Small editor utilities -*- lexical-binding: t; -*-

(use-package gcmh
  :defer 1
  :diminish (gcmh-mode)
  :commands (gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto)
  (gcmh-mode))

(use-package browse-kill-ring
  :defer t
  :commands (browse-kill-ring))

(use-package emacs-everywhere
  :config
  (setq emacs-everywhere--dir
        (locate-user-emacs-file "everywhere")))

(use-package autorevert
  :ensure nil
  :hook ((after-init . global-auto-revert-mode)
         (dired-mode . auto-revert-mode))
  :custom
  (auto-revert-use-notify nil)
  (auto-revert-verbose t))

(use-package expand-region
  :defer t)

(use-package saveplace
  :ensure nil
  :defer 1
  :config
  (setq-default save-place t)
  (setq save-place-file (locate-user-emacs-file "places"))
  (save-place-mode 1))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :commands (recentf-mode recentf-add-file recentf-apply-filename-handlers)
  :custom
  (recentf-auto-cleanup nil)
  (recentf-max-saved-items 100)
  (recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG")))

(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (savehist-additional-variables
   '(file-name-history
     kmacro-ring
     compile-history
     compile-command))
  (savehist-autosave-interval 60)
  (savehist-ignored-variables
   '(load-history
     flyspell-auto-correct-ring
     org-roam-node-history
     magit-revision-history
     org-read-date-history
     query-replace-history
     yes-or-no-p-history
     kill-ring))
  :config
  (savehist-mode 1))

(provide 'nima-feature-editor-utils)
;;; editor-utils.el ends here
