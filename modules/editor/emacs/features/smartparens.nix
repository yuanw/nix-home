{
  epkgs = epkgs: [
    epkgs.smartparens
  ];

  elisp = ''
    (use-package smartparens
      :defer 3
      :commands (smartparens-global-mode show-smartparens-global-mode)
      :config
      (require 'smartparens-config)
      (smartparens-global-mode t)
      (show-smartparens-global-mode t)
      (keymap-set smartparens-mode-map "M-<right>" #'sp-forward-sexp)
      (keymap-set smartparens-mode-map "M-<left>" #'sp-backward-sexp))
  '';
}
