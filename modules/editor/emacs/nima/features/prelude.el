;;; prelude.el --- Base configuration for nima Emacs -*- lexical-binding: t; -*-

;; Load dependencies on which all features can depend.
(require 'cl-lib)
(require 'seq)
(require 'xdg)

(eval-when-compile
  (require 'use-package))

;; Define directories that all features can use.
(defconst my-emacs-data-directory (expand-file-name "emacs/" (xdg-data-home))
  "A directory to store Emacs data such as backup files and autosave files.")
(defconst my-emacs-state-directory (expand-file-name "emacs/" (xdg-state-home))
  "A directory to store Emacs state such as history and recent files.")
(defconst my-emacs-cache-directory (expand-file-name "emacs/" (xdg-cache-home))
  "A directory to store Emacs cache files.")

;; Compatibility with imported wonima snippets.
(defconst wonima-emacs-data-directory my-emacs-data-directory
  "Compatibility alias for imported wonima feature snippets.")
(defconst wonima-emacs-state-directory my-emacs-state-directory
  "Compatibility alias for imported wonima feature snippets.")
(defconst wonima-emacs-cache-directory my-emacs-cache-directory
  "Compatibility alias for imported wonima feature snippets.")

(dolist (dir (list my-emacs-data-directory
                   my-emacs-state-directory
                   my-emacs-cache-directory))
  (make-directory dir t))

;; Disable some GUI distractions.  We set frame defaults as early as this
;; staged nima prelude is loaded, and also disable the modes for the current
;; frame when their functions are available.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(add-to-list 'default-frame-alist '(undecorated-round . t))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Set up fonts. These mirror the repository defaults from `config.my.*`.
(let ((mono-spaced-font "PragmataPro VF Mono Liga")
      (proportionately-spaced-font "PragmataPro Liga"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 180)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; Disable startup message.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name))

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Don't blink the cursor.
(setq blink-cursor-mode nil)

;; Set frame title.
(setq frame-title-format
      '("" invocation-name ": " (:eval
                              (if (buffer-file-name)
                                  (abbreviate-file-name (buffer-file-name))
                                "%b"))))

;; Make sure the mouse cursor is visible at all times.
(set-face-background 'mouse "#ffffff")

;; Accept 'y' and 'n' rather than 'yes' and 'no'.
(setopt use-short-answers t)
(setopt use-dialog-box nil)

;; Don't want to move based on visual line.
(setq line-move-visual nil)

;; Stop creating backup and autosave files.
(setopt make-backup-files nil
        auto-save-default nil)

;; Default is 4k, which is too low for LSP.
(setq read-process-output-max (* 1024 1024))

;; Always show line and column number in the mode line.
(line-number-mode)
(column-number-mode)
(global-visual-line-mode t)

;; Enable some features that are disabled by default.
(dolist (cmd '(narrow-to-region
               upcase-region
               downcase-region
               dired-find-alternate-file
               LaTeX-narrow-to-environment
               TeX-narrow-to-group
               narrow-to-page
               set-goal-column
               scroll-left
               scroll-right))
  (put cmd 'disabled nil))

;; Minimising & quitting Emacs way too many times without wanting to.
(put 'suspend-frame 'disabled t)

;; Use UTF-8.
(prefer-coding-system 'utf-8)

;; Nicer handling of regions.
(transient-mark-mode 1)

;; Avoid noisy bell.
(setq visible-bell t)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)
(setq xref-search-program 'ripgrep)

(defun my/locate-hm-init ()
  "Locate hm-init file."
  (interactive)
  (let ((first-path (seq-find (lambda (s) (string-prefix-p "/nix/store/" s)) load-path)))
    (if first-path
        (find-alternate-file
         (concat (car (split-string first-path "/share/emacs/site-lisp"))
                 "/share/emacs/site-lisp/hm-init.el"))
      (error "cannot find /nix/store in load-path"))))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
The generic `keyboard-quit' does not do the expected thing when the
minibuffer is open. Whereas we want it to close the minibuffer, even
without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the *Completions* buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(defun add-all-to-list (var &rest elems)
  "Add ELEMS to list variable VAR."
  (dolist (elem (reverse elems))
    (add-to-list var elem)))

;; I rarely close Emacs. It is annoying to accidentally close Emacs, so warn first.
(setq confirm-kill-emacs #'y-or-n-p)

(provide 'nima-feature-prelude)
;;; prelude.el ends here
