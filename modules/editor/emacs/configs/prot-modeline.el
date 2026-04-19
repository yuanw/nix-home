;;; prot-modeline-config.el --- Prot Modeline configuration -*- lexical-binding: t; -*-
;;
;; Hot-reloadable prot-modeline config — edit this file and reload with:
;;   M-x hm/reload-config
;; or enable auto-reload with:
;;   M-x hm-hot-reload-mode

(setq mode-line-compact nil) ; Emacs 28
(setq mode-line-right-align-edge 'right-margin) ; Emacs 30

;; ── Meow indicator ─────────────────────────────────────────────────
;; Style meow's state indicator faces to match prot-modeline's look.
;; Each state gets a colored background using prot-modeline indicator faces.
(set-face-attribute 'meow-normal-indicator nil
                    :inherit 'prot-modeline-indicator-green-bg)
(set-face-attribute 'meow-insert-indicator nil
                    :inherit 'prot-modeline-indicator-yellow-bg)
(set-face-attribute 'meow-motion-indicator nil
                    :inherit 'prot-modeline-indicator-blue-bg)
(set-face-attribute 'meow-keypad-indicator nil
                    :inherit 'prot-modeline-indicator-red-bg)
(set-face-attribute 'meow-beacon-indicator nil
                    :inherit 'prot-modeline-indicator-magenta-bg)

;; Suppress meow's built-in minor mode lighters (" [N]", " [I]", etc.)
;; since we display the state via `(:eval (meow-indicator))' above.
(dolist (mode '(meow-normal-mode meow-insert-mode meow-motion-mode
                meow-keypad-mode meow-beacon-mode))
  (when-let* ((entry (assq mode minor-mode-alist)))
    (setcdr entry (list nil))))

;; ── Mode-line format ───────────────────────────────────────────────
(setq-default mode-line-format
     '("%e"
       (:eval (meow-indicator))           ; meow state indicator
       prot-modeline-kbd-macro
       prot-modeline-narrow
       prot-modeline-buffer-status
       prot-modeline-window-dedicated-status
       prot-modeline-input-method
       "  "
       prot-modeline-buffer-identification
       "  "
       prot-modeline-major-mode
       "  "
       mode-line-position
       "  "
       prot-modeline-process
       "  "
       prot-modeline-vc-branch
       "  "
       prot-modeline-eglot
       "  "
       prot-modeline-flymake
       "  "
       mode-line-format-right-align ; Emacs 30
       prot-modeline-notmuch-indicator
       "  "
       prot-modeline-misc-info))

(with-eval-after-load 'spacious-padding
  (defun prot/modeline-spacious-indicators ()
    "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
    (if (bound-and-true-p spacious-padding-mode)
        (set-face-attribute 'prot-modeline-indicator-button nil :box t)))

  ;; Run it at startup and then afterwards whenever
  ;; `spacious-padding-mode' is toggled on/off.
  (prot/modeline-spacious-indicators)
  (add-hook 'spacious-padding-mode-hook #'prot/modeline-spacious-indicators))

;;; prot-modeline-config.el ends here