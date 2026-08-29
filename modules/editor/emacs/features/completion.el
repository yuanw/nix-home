;;;;; completion style

(require 'cl-lib)

;; References:
;; - Prot's completion module: thoughtful category defaults and minibuffer polish.
;; - Omar Antolín's config: small, focused Orderless/Vertico/Cape setup.
;; - Karthink's setup: practical dabbrev/hippie-expand defaults.

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion))
     (eglot (styles orderless basic))))
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (orderless-smart-case nil)
  (orderless-style-dispatchers
   '(orderless-affix-dispatch prefixes-for-separators))
  :config
  (setq-default case-fold-search t)
  (defun prefixes-for-separators (pattern _index _total)
    "Use prefix matching for path-like Orderless components."
    (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
      (cons 'orderless-prefixes pattern)))
  (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
  (when (fboundp 'orderless-kwd-dispatch)
    (cl-pushnew #'orderless-kwd-dispatch orderless-style-dispatchers))
  (when (>= emacs-major-version 31)
    (setq completion-pcm-leading-wildcard t)))

;;;;; minibuffer completion

(setopt read-extended-command-predicate #'command-completion-default-include-p
        enable-recursive-minibuffers t
        read-answer-short t
        use-short-answers t
        minibuffer-default-prompt-format " [%s]"
        resize-mini-windows t
        minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

(minibuffer-depth-indicate-mode 1)
(when (fboundp 'minibuffer-electric-default-mode)
  (minibuffer-electric-default-mode 1))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(file-name-shadow-mode 1)

;; Emacs 31+ has builtin CRM prompt support. Keep wonima's patch only on older
;; Emacs where it is still useful.
(when (< emacs-major-version 31)
  (defun wonima--prepend-crm-indicator (args)
    "Prepend an indicator to the prompt of `completing-read-multiple'."
    (defvar crm-separator)
    (cons (format "[%s-separated list] %s"
                  (string-replace "[ \t]*" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'wonima--prepend-crm-indicator))

(setopt vertico-cycle t
        vertico-count 15
        vertico-resize nil)
(vertico-mode 1)

(with-eval-after-load 'vertico
  (defvar vertico-map)
  (keymap-set vertico-map "<prior>" #'vertico-scroll-down)
  (keymap-set vertico-map "<next>" #'vertico-scroll-up)
  (when (require 'vertico-directory nil t)
    (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
    (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

(marginalia-mode 1)
(with-eval-after-load 'marginalia
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle))

;;;;; in-buffer completion

(setopt tab-always-indent 'complete)

(when (fboundp 'global-completion-preview-mode)
  (setopt completion-preview-exact-match-only nil
          completion-preview-minimum-symbol-length 4
          completion-preview-idle-delay 0.3
          completion-preview-ignore-case t)
  (global-completion-preview-mode 1))

(setopt corfu-cycle t
        corfu-min-width 20
        corfu-preview-current nil
        corfu-popupinfo-delay '(1.0 . 0.4)
        corfu-quit-no-match 'separator)

(with-eval-after-load 'corfu
  (defvar corfu-map)
  (declare-function corfu-insert-separator "corfu")
  ;; Orderless component separator in Corfu, matching the minibuffer behavior.
  (keymap-set corfu-map "SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "<tab>" #'corfu-complete))

(global-corfu-mode 1)
(corfu-popupinfo-mode 1)
(corfu-history-mode 1)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Emacs 31+ has better terminal child-frame support, so corfu-terminal is only
;; hooked when the package provides the mode and the running Emacs still needs it.
(when (and (< emacs-major-version 31) (fboundp 'corfu-terminal-mode))
  (add-hook 'tty-setup-hook #'corfu-terminal-mode))

;;;;; completion-at-point helpers

(defun my/completion-add-local-capfs (&rest capfs)
  "Append CAPFS to the buffer-local `completion-at-point-functions'."
  (dolist (capf (reverse capfs))
    (add-hook 'completion-at-point-functions capf 90 t)))

(add-hook 'prog-mode-hook
          (lambda ()
            (my/completion-add-local-capfs #'cape-file #'cape-dabbrev)))
(add-hook 'text-mode-hook
          (lambda ()
            (my/completion-add-local-capfs #'cape-file #'cape-dabbrev)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (my/completion-add-local-capfs #'cape-elisp-block)))

(keymap-global-set "C-c p" 'cape-prefix-map)

;; TODO maybe also bind `cape-history' for eshell and shell
(dolist (command '(previous-matching-history-element next-matching-history-element))
  (keymap-set minibuffer-mode-map
              (format "<remap> <%s>" (symbol-name command))
              #'cape-history))

;;;;; dabbrev and hippie-expand

(with-eval-after-load 'dabbrev
  (setopt dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
          dabbrev-abbrev-skip-leading-regexp "[$*/=~']"
          dabbrev-backward-only nil
          dabbrev-case-distinction 'case-replace
          dabbrev-case-fold-search nil
          dabbrev-case-replace 'case-replace
          dabbrev-check-other-buffers t
          dabbrev-eliminate-newlines t
          dabbrev-upcase-means-case-search t)
  (defvar dabbrev-ignored-buffer-modes)
  (dolist (mode '(archive-mode authinfo-mode doc-view-mode image-mode pdf-view-mode))
    (cl-pushnew mode dabbrev-ignored-buffer-modes)))

(with-eval-after-load 'hippie-exp
  (setopt hippie-expand-try-functions-list
          '(try-expand-dabbrev-visible
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol)))

(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "C-M-/" #'dabbrev-completion)

(provide 'nima-feature-completion)
