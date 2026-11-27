{
  epkgs = epkgs: [
    epkgs.nerd-icons
  ];

  elisp = ''
    (use-package nerd-icons
      :defer t
      :init
      ;; VF Mono Liga (0.903 instance) has newer MD glyphs (png/gif) that the
      ;; static Mono Liga cut lacks. Default face stays on static Mono Liga;
      ;; only icon glyphs use this family via nerd-icons-set-font.
      (setq nerd-icons-font-family "PragmataPro VF Mono Liga")
      ;; Not `:if (display-graphic-p)': under `--fg-daemon' that is nil at
      ;; init and would skip the package for later GUI frames too.
      (defun my/nerd-icons-set-font ()
        "Remap nerd-icons glyphs only on graphical frames."
        (when (display-graphic-p)
          (nerd-icons-set-font)))
      :hook (server-after-make-frame . my/nerd-icons-set-font))
  '';
}
