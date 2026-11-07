;;; Commentary: direnv.el --- Direnv integration -*- lexical-binding: t; -*-
;;; Code:
(use-package direnv
  :functions (direnv--maybe-update-environment)
  :preface
  ;; Not just Emacs: this is also the profile that provides `iets' and `direnv',
  ;; and the .envrc's `use flake' fails (silently, exit 0) when they are off the
  ;; PATH Emacs hands to `direnv export'.  Fall back to the running Emacs's own
  ;; bin dir so the patch still runs when emacsclient is not on PATH at startup.
  (defconst emacs-binary-path
    (directory-file-name
     (file-name-directory
      (or (executable-find "emacsclient")
          (expand-file-name "emacs" invocation-directory)))))

  (defun patch-direnv-environment (&rest _args)
    (let ((dir (file-name-as-directory emacs-binary-path)))
      (unless (member dir exec-path)
        (setenv "PATH" (concat emacs-binary-path ":" (getenv "PATH")))
        (setq exec-path (cons dir exec-path)))))

  (defvar my-direnv-idle-timer nil)

  (defun my-direnv-maybe-update (&rest _ignore)
    (when my-direnv-idle-timer
      (cancel-timer my-direnv-idle-timer))
    (setq my-direnv-idle-timer
          (run-with-idle-timer 0.3 nil #'direnv--maybe-update-environment)))

  (defun my-magit-worktree-p (directory)
    (let ((git (expand-file-name ".git" directory)))
      (and (file-exists-p git) (not (file-directory-p git)))))

  (defun my-magit-worktree-maybe-copy-envrc (source-root target-dir)
    (let ((src (expand-file-name ".envrc" source-root))
          (dst (expand-file-name ".envrc" target-dir)))
      (when (and (file-readable-p src)
                 (not (file-exists-p dst)))
        (copy-file src dst t)
        (message "Copied .envrc to %s" target-dir)
        ;; A new path is not in direnv's allow list, so the bare copy loads
        ;; nothing.  The source .envrc is already allowed, and it updates the
        ;; environment afterwards.
        (when (fboundp 'direnv-allow)
          (let ((default-directory target-dir))
            (direnv-allow))))))

  (defun my-magit-worktree-after-create-copy-envrc (fn &rest args)
    (let* ((target (car args))
           (source (magit-toplevel)))
      (apply fn args)
      (when (and target source (my-magit-worktree-p target))
        (my-magit-worktree-maybe-copy-envrc source target))))

  :init
  (advice-add 'direnv-update-directory-environment
              :after #'patch-direnv-environment)
  (add-hook 'change-major-mode-hook #'my-direnv-maybe-update)
  ;; (add-hook 'buffer-list-update-hook #'my-direnv-maybe-update)
  (add-hook 'window-selection-change-functions #'my-direnv-maybe-update)

  (direnv-mode 1)

  (with-eval-after-load 'magit-worktree
    (advice-add #'magit-worktree-checkout
                :around #'my-magit-worktree-after-create-copy-envrc)
    (advice-add #'magit-worktree-branch
                :around #'my-magit-worktree-after-create-copy-envrc)))

(provide 'nima-feature-direnv)
;;; direnv.el ends here

