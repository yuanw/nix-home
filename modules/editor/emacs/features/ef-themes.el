;;; ef-themes.el --- Ef themes configuration -*- lexical-binding: t; -*-

(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode)
  :preface
  (defun my/select-light-theme ()
    "Select the light Ef theme."
    (interactive)
    (ef-themes-select 'ef-day))

  (defun my/select-dark-theme ()
    "Select the dark Ef theme."
    (interactive)
    (ef-themes-select 'ef-owl))

  :custom
  (custom-safe-themes t)
  (modus-themes-common-palette-overrides '((fringe unspecified)))
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-bold-constructs t)
  (modus-themes-to-toggle '(ef-owl ef-spring))
  :bind (("C-c t l" . my/select-light-theme)
         ("C-c t d" . my/select-dark-theme)
         ("C-c t t" . modus-themes-toggle))
  :config
  ;; Fix for newer Emacs: prefer .el files over potentially incompatible .elc files.
  (setq load-prefer-newer t)

  ;; If you like two specific themes and want to switch between them, specify
  ;; them in `ef-themes-to-toggle' and invoke `ef-themes-toggle'.
  ;;(setq ef-themes-to-toggle '(ef-day ef-owl))
  (setq ef-themes-headings
        '((0 variable-pitch light 1.9)
          (1 variable-pitch light 1.8)
          (2 variable-pitch regular 1.7)
          (3 variable-pitch regular 1.6)
          (4 variable-pitch regular 1.5)
          (5 variable-pitch 1.4)
          (6 variable-pitch 1.3)
          (7 variable-pitch 1.2)
          (t variable-pitch 1.1)))

  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

  ;; Disable all other themes to avoid awkward blending.
  ;;(mapc #'disable-theme custom-enabled-themes)
  (setq org-startup-with-inline-images t)

  ;; Load theme after display initialization.
  ;;(load-theme 'ef-owl :no-confirm)
  )

(provide 'nima-feature-ef-themes)
;;; ef-themes.el ends here
