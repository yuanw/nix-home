;;; lean-templates.el --- Tempel templates for Lean (Nael mode) -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural Lean templates for Tempel, contributed to
;; `tempel-template-sources' in Nael buffers only.  Tempel is installed and
;; bound by features/tempel.nix; this file only adds a buffer-local template
;; source, so it needs no package of its own.
;;
;; Triggers (bindings come from features/tempel.el): type the template name
;; and press TAB (`tempel-expand'), or `M-+' for `tempel-complete', or `M-*'
;; for `tempel-insert'.  Type `eval', not `#eval': `#' is punctuation in
;; Nael's syntax table, so the trigger prefix never contains it.
;;
;; Add more templates by appending (NAME ELEMENTS... :ann "DOC") entries to
;; `my-lean-templates--templates'; see `tempel--element' for the element
;; syntax (`p' field, `n' newline, `>' indent, `q' finish).

;;; Code:

(defun my-lean-templates--templates ()
  "Return the Lean templates for `nael-mode'.
Suitable as a `tempel-template-sources' function."
  '((eval "#eval " (p "expression") :ann "Lean #eval")
    (check "#check " (p "expression") :ann "Lean #check")
    (structure "structure " (p "Name") " where"
               "\n  " (p "x") " : " (p "Type")
               "\n  " (p "y") " : " (p "Type2")
               :ann "Lean structure ... where")))

(defun my-lean-templates--setup ()
  "Buffer-locally contribute the Lean templates to Tempel.
`add-to-list' has no LOCAL argument, so set the buffer-local value."
  (setq-local tempel-template-sources
              (cons #'my-lean-templates--templates
                    tempel-template-sources)))

;; Add the hook only once Tempel is loaded, so that `tempel-template-sources'
;; has its real default value when we cons onto it.
(with-eval-after-load 'tempel
  (add-hook 'nael-mode-hook #'my-lean-templates--setup))

(provide 'nima-feature-lean-templates)
;;; lean-templates.el ends here
