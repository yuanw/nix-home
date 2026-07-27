;;; prot-modeline-config.el --- Prot Modeline configuration -*- lexical-binding: t; -*-
;;
;; Hot-reloadable prot-modeline config — edit this file and reload with:
;;   M-x hm/reload-config
;; or enable auto-reload with:
;;   M-x hm-hot-reload-mode

(setq mode-line-compact nil) ; Emacs 28
(setq mode-line-right-align-edge 'right-margin) ; Emacs 30

(defvar my/tty-mode-line-format
  '("%b %f  %l:%c  %m")
  "Simple mode line for terminal frames (emacsclient -nw).")

;; ── Mode-line format ───────────────────────────────────────────────
;; The meow/hel indicator is added separately by prot-modeline-meow.el
;; or prot-modeline-hel.el when modal editing is enabled.
(defvar my/graphical-mode-line-format
  '("%e"
    ""
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
    prot-modeline-misc-info)
  "Prot modeline format for graphical frames.")

(defun my/mode-line-format-for-frame (&optional frame)
  "Return an appropriate `mode-line-format' for FRAME."
  (if (display-graphic-p (or frame (selected-frame)))
      my/graphical-mode-line-format
    my/tty-mode-line-format))

(defun my/ensure-mode-line-for-frame (&optional frame)
  "Use prot-modeline on graphical frames; a plain line on terminals."
  (when-let ((frame (or frame (selected-frame))))
    (let ((format (my/mode-line-format-for-frame frame)))
      (dolist (buf (buffer-list))
        (when (get-buffer-window buf frame)
          (with-current-buffer buf
            (unless (equal mode-line-format format)
              (setq mode-line-format format))))))))

;; Safe default: terminal-friendly. Graphical frames opt in via the hook.
(setq-default mode-line-format my/tty-mode-line-format)

(when-let ((frame (selected-frame)))
  (my/ensure-mode-line-for-frame frame))

(add-hook 'server-after-make-frame-hook #'my/ensure-mode-line-for-frame)
(add-hook 'window-configuration-change-hook #'my/ensure-mode-line-for-frame)

(with-eval-after-load 'spacious-padding
  (defun prot/modelline-spacious-indicators ()
    "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
    (when (display-graphic-p)
      (if (bound-and-true-p spacious-padding-mode)
          (set-face-attribute 'prot-modeline-indicator-button nil :box t))))

  ;; Run it at startup and then afterwards whenever
  ;; `spacious-padding-mode' is toggled on/off.
  (when (display-graphic-p)
    (prot/modelline-spacious-indicators))
  (add-hook 'spacious-padding-mode-hook #'prot/modelline-spacious-indicators))

;;; prot-modeline-config.el ends here
