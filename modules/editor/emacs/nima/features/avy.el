;;; avy.el --- Avy navigation -*- lexical-binding: t; -*-

(use-package avy
  :bind (("C-c j" . avy-goto-char-timer)
         ("M-g y" . avy-copy-line)
         ("M-g M-y" . avy-copy-region)
         ("M-g M-p" . avy-goto-line-above)
         ("M-g M-n" . avy-goto-line-below)
         ("M-g C-w" . avy-kill-region)
         ("M-g M-w" . avy-kill-ring-save-region))
  :config
  (setq avy-keys '(?r ?h ?a ?c ?i ?e ?t ?n ?s ?k)))

(provide 'nima-feature-avy)
;;; avy.el ends here
