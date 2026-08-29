;;; consult.el --- Consult configuration -*- lexical-binding: t; -*-

(defcustom my/consult-ripgrep-or-line-limit 300000
  "Buffer size threshold for `my/consult-ripgrep-or-line'."
  :group 'consult
  :type 'integer)

(defun my/consult-ripgrep-or-line (&optional arg)
  "Call `consult-line' for small buffers, `consult-ripgrep' for large files.
With prefix ARG, forward to `consult-line-multi' for small buffers.  This is
adapted from Karthink's Consult setup."
  (interactive "p")
  (if (or (not buffer-file-name)
          (buffer-narrowed-p)
          (file-remote-p buffer-file-name)
          (and (fboundp 'jka-compr-get-compression-info)
               (jka-compr-get-compression-info buffer-file-name))
          (<= (buffer-size)
              (/ my/consult-ripgrep-or-line-limit
                 (if (eq major-mode 'org-mode) 4 1))))
      (pcase arg
        (4 (consult-line-multi nil))
        (16 (consult-line-multi t))
        (_ (consult-line)))
    (when (file-writable-p buffer-file-name)
      (save-buffer))
    (consult-ripgrep)))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . my/consult-ripgrep-or-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 3)
  (consult-async-input-debounce 0.5)
  (consult-async-input-throttle 0.8)
  :init
  ;; Tweak register preview for `consult-register-load',
  ;; `consult-register-store' and built-in commands.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview and completion-in-region.
  (setq completion-in-region-function #'consult-completion-in-region
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (keymap-set search-map "l" #'my/consult-ripgrep-or-line)
  ;; `consult-narrow-map' is active after `consult-narrow-key', so bind the
  ;; help key inside that map rather than trying to bind the invalid key string
  ;; "<?".
  (keymap-set consult-narrow-map "?" #'consult-narrow-help)
  (use-package consult-xref)
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   my/consult-ripgrep-or-line
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-man
   consult-bookmark
   consult-recent-file
   consult-xref
   :preview-key '(:debounce 0.4 any)))

(use-package consult-project-extra
  :bind (("C-c p f" . consult-project-extra-find)
         ("C-c p o" . consult-project-extra-find-other-window)))

(provide 'nima-feature-consult)
;;; consult.el ends here
