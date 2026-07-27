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
  (setq hel-mode-line-info nil)

  ;; Insert the hel state indicator into the graphical mode-line format only.
  (setq my/graphical-mode-line-format
        (cl-substitute '(:eval (prot-modeline--hel-state)) ""
                       my/graphical-mode-line-format
                       :test 'equal :count 1))

  (when-let ((frame (selected-frame)))
    (my/ensure-mode-line-for-frame frame)))

;;; prot-modeline-hel.el ends here
