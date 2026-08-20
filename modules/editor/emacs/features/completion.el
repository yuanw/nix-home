;;;;; completion style

(require 'cl-lib)

;; 'basic is needed by dynamic completion tables
(setopt completion-styles '(orderless basic))

;; prepend 'partial-completion since it is useful for completing file paths
;; in addition, it is needed for `find-file' to open multiple files at once using wildcards
(setopt completion-category-overrides '((file (styles partial-completion))))

(with-eval-after-load 'orderless
  (defvar orderless-style-dispatchers)
  (when (fboundp 'orderless-kwd-dispatch)
    (cl-pushnew #'orderless-kwd-dispatch orderless-style-dispatchers)))

;;;;; minibuffer completion

(setopt savehist-file (expand-file-name "history" wonima-emacs-state-directory))
(savehist-mode)
(setopt history-length 200
        history-delete-duplicates t)

;; filter out commands un-related to the current buffer for M-x
(setopt read-extended-command-predicate #'command-completion-default-include-p)

(setopt enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; keep cursor outside of the minibuffer prompt
(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

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

(setopt vertico-cycle t)
(vertico-mode)

(marginalia-mode)

;;;;; in-buffer completion

(setopt tab-always-indent 'complete)

(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode))

(setopt corfu-preview-current nil)
;; NOTE missing candidates from language servers with or without orderless
;;   - language servers probably won't send all candidates at once
;;   - corfu won't update candidates from language servers after `corfu-insert-separator'
;; see also the corfu wiki section "Configuring Corfu for Eglot"
(with-eval-after-load 'corfu
  (defvar corfu-map)
  (declare-function corfu-insert-separator "corfu")
  ;; better orderless integration
  (keymap-set corfu-map "SPC" #'corfu-insert-separator))
(global-corfu-mode)
(corfu-popupinfo-mode)
(corfu-history-mode)

;; Emacs 31+ has better terminal child-frame support, so corfu-terminal is only
;; hooked when the package provides the mode and the running Emacs still needs it.
(when (and (< emacs-major-version 31) (fboundp 'corfu-terminal-mode))
  (add-hook 'tty-setup-hook #'corfu-terminal-mode))

(with-eval-after-load 'dabbrev
  (defvar dabbrev-ignored-buffer-modes)
  (dolist (mode '(authinfo-mode doc-view-mode))
    (cl-pushnew mode dabbrev-ignored-buffer-modes)))
;; swap keybindings to prefer completion
(keymap-global-set "M-/" #'dabbrev-completion)
(keymap-global-set "C-M-/" #'dabbrev-expand)

(keymap-global-set "C-c p" 'cape-prefix-map)
(add-hook 'completion-at-point-functions #'cape-file)
;; TODO maybe also bind `cape-history' for eshell and shell
(dolist (command '(previous-matching-history-element next-matching-history-element))
  (keymap-set minibuffer-mode-map
              (format "<remap> <%s>" (symbol-name command))
              #'cape-history))

(provide 'nima-feature-completion)
