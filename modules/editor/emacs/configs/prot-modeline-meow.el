;;; prot-modeline-meow.el --- Meow indicator for prot-modeline -*- lexical-binding: t; -*-
;;
;; Loaded conditionally by modal-edit.nix when modalEditing == "meow".

(defun prot-modeline--meow-or-emacs ()
  "Return a mode-line indicator for the current modal state.
Shows the Meow state when `meow-mode' is active, otherwise shows EMACS."
  (cond
   ;; Meow is active in this buffer: delegate to its indicator
   ((and (fboundp 'meow-indicator) meow-mode)
    (meow-indicator))
   ;; Meow is installed but not active here
   ((fboundp 'meow-indicator)
    (propertize " EMACS " 'face 'prot-modeline-indicator-gray-bg))
   ;; Meow not installed at all
   (t nil)))

(with-eval-after-load 'meow
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
  ;; since we display the state via `prot-modeline--meow-or-emacs'.
  (dolist (mode '(meow-normal-mode meow-insert-mode meow-motion-mode
                  meow-keypad-mode meow-beacon-mode))
    (when-let* ((entry (assq mode minor-mode-alist)))
      (setcdr entry (list nil))))

  ;; Insert the meow indicator into the mode-line format (replacing the empty string)
  (setq-default mode-line-format
                (cl-substitute
                 '(:eval (prot-modeline--meow-or-emacs)) ""
                 (default-value 'mode-line-format)
                 :test 'equal :count 1)))

;;; prot-modeline-meow.el ends here
