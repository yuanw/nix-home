{
  epkgs = epkgs: [ epkgs.reader ];

  elisp = ''
    (use-package reader
      :defer t
      :mode (("\\.pdf\\'" . reader-mode))
      :config
      ;; reader.el re-registers every format it supports, epub included, when it
      ;; loads.  Keep epub on nov, which shows metadata and covers properly.
      (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
  '';
}
