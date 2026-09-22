;;; magit.el --- Magit configuration -*- lexical-binding: t; -*-

(use-package magit
  :demand t
  :commands (magit-project-status)
  :bind (("C-x g" . magit-status)
         :map magit-mode-map
         ("U" . magit-unstage-all)
         ("k" . magit-discard)
         ("C-c H" . magit-hunk-dwim))
  :config
  (setq magit-list-refs-sortby "-committerdate")
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)

  ;; ponytail: Alacritty-only host; eat/vterm if OpenTUI ever runs cleanly inside Emacs
  (defun magit-hunk--run (&rest args)
    "Run hunk ARGS in Alacritty at the Magit repository root."
    (let ((default-directory (magit-toplevel))
          (hunk (executable-find "hunk"))
          (term (executable-find "alacritty")))
      (unless hunk (user-error "hunk executable not found"))
      (unless term (user-error "alacritty executable not found"))
      (apply #'start-process "hunk" nil term "-e" hunk args)))

  (defun magit-hunk-diff (&optional staged)
    "Review the working tree in Hunk.
With prefix argument STAGED, review staged changes only."
    (interactive "P")
    (if staged
        (magit-hunk--run "diff" "--staged")
      (magit-hunk--run "diff")))

  (defun magit-hunk-show (&optional rev)
    "Review REV (commit at point, else HEAD) in Hunk."
    (interactive (list (or (magit-commit-at-point) "HEAD")))
    (magit-hunk--run "show" rev))

  (defun magit-hunk-dwim (&optional staged)
    "Review the commit at point in Hunk, else the working tree.
With prefix argument STAGED (and no commit at point), review staged changes."
    (interactive "P")
    (if-let ((rev (magit-commit-at-point)))
        (magit-hunk--run "show" rev)
      (magit-hunk-diff staged)))

  (with-eval-after-load 'magit-diff
    (transient-append-suffix 'magit-diff "d"
      '("H" "hunk" magit-hunk-diff))))

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
