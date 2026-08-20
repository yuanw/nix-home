{
  epkgs = epkgs: [ epkgs.spacious-padding ];
  elisp = ''
    (use-package spacious-padding
      :commands (spacious-padding-mode)
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
