{
  epkgs = epkgs: [
    epkgs.tempel
  ];

  elisp = ''
    ;; Configure Tempel
    (use-package tempel
      :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
             ("M-*" . tempel-insert))

      :init

      ;; Setup completion at point
      (defun tempel-setup-capf ()
        ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
        ;; only triggers on exact matches. We add `tempel-expand' *before* the main
        ;; programming mode Capf, such that it will be tried first.
        (setq-local completion-at-point-functions
                    (cons #'tempel-expand completion-at-point-functions))

        ;; Alternatively use `tempel-complete' if you want to see all matches.  Use
        ;; a trigger prefix character in order to prevent Tempel from triggering
        ;; unexpectly.
        ;; (setq-local corfu-auto-trigger "/"
        ;;             completion-at-point-functions
        ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
        ;;                   completion-at-point-functions))
        )

      (add-hook 'conf-mode-hook 'tempel-setup-capf)
      (add-hook 'prog-mode-hook 'tempel-setup-capf)
      (add-hook 'text-mode-hook 'tempel-setup-capf)

      ;; Optionally make the Tempel templates available to Abbrev,
      ;; either locally or globally. `expand-abbrev' is bound to C-x '.
      ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
      ;; (global-tempel-abbrev-mode)
      )

  '';
}
