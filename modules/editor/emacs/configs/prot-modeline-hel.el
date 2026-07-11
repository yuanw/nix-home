;;; prot-modeline-hel.el --- Hel state indicator for prot-modeline -*- lexical-binding: t; -*-
;;
;; Loaded conditionally by modal-edit.nix when modalEditing == "hel".

(defvar prot-modeline--hel-state-colors
  '((normal . prot-modeline-indicator-green-bg)
    (insert . prot-modeline-indicator-yellow-bg)
    (emacs  . prot-modeline-indicator-gray-bg))
  "Alist of Hel states to prot-modeline indicator faces.")

(defun prot-modeline--hel-state ()
  "Return a mode-line indicator for the current Hel state.
Shows the Hel state when `hel-mode' is active, otherwise shows nothing."
  (when (and (bound-and-true-p hel-local-mode) hel-state)
    (let* ((state-name (capitalize (symbol-name hel-state)))
           (face (or (alist-get hel-state prot-modeline--hel-state-colors)
                     'prot-modeline-indicator-gray-bg)))
      (propertize (format " %s " state-name) 'face face))))

(with-eval-after-load 'hel
  ;; Suppress hel's own mode-line-misc-info entry (multiple cursors / search)
  ;; since we display the state here instead. Users can still see search
  ;; progress via the built-in mechanism if desired.
  (setq hel-mode-line-info nil)

  ;; Insert the hel state indicator into the mode-line format
  ;; (replacing the empty string placeholder from prot-modeline.el).
  (setq-default mode-line-format
                (cl-substitute
                 '(:eval (prot-modeline--hel-state)) ""
                 (default-value 'mode-line-format)
                 :test 'equal :count 1)))

;;; prot-modeline-hel.el ends here
