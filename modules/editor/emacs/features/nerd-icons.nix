{
  epkgs = epkgs: [
    epkgs.nerd-icons
  ];

  elisp = ''
    (use-package nerd-icons
      :defer t
      :init
      (setq nerd-icons-font-family "PragmataPro Mono Liga")
      ;; Not `:if (display-graphic-p)': under `--fg-daemon' that is nil at
      ;; init and would skip the package for later GUI frames too.
      (defun my/nerd-icons-set-font ()
        "Remap nerd-icons glyphs only on graphical frames."
        (when (display-graphic-p)
          (nerd-icons-set-font)))
      :hook (server-after-make-frame . my/nerd-icons-set-font))
  '';
}
