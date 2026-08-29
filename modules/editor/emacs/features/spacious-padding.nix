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
      :config
      (spacious-padding-mode 1))
  '';
}
