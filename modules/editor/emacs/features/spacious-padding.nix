{
  epkgs = epkgs: [ epkgs.spacious-padding ];
  elisp = ''
    (use-package spacious-padding
      ;; Load eagerly so the header-line padding is active before/with
      ;; `keycast-header-line-mode', matching the old Home Manager setup.
      :demand t
      :init
      ;; Same padding as the old/main-branch Home Manager use-package config.
      (setq spacious-padding-widths
            '( :internal-border-width 15
               :header-line-width 4
               :mode-line-width 3
               :tab-width 4
               :right-divider-width 15
               :scroll-bar-width 8
               :fringe-width 8))
      ;; Not `:if (display-graphic-p)': under `--fg-daemon' that is nil at
      ;; init and would skip the package for later GUI frames too.
      (defun my/spacious-padding-enable-on-gui ()
        "Enable spacious-padding only once a graphical frame exists."
        (when (and (display-graphic-p) (not spacious-padding-mode))
          (spacious-padding-mode 1)))
      :hook (server-after-make-frame . my/spacious-padding-enable-on-gui)
      :config
      (my/spacious-padding-enable-on-gui))
  '';
}
