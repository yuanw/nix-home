{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  epkgs = epkgs: [
    epkgs.lsp-ui
  ];

  elisp = ''
    (use-package lsp-ui
      :commands (lsp-ui-mode)
      :config
      (setq lsp-ui-sideline-enable t
            lsp-ui-sideline-show-symbol nil
            lsp-ui-sideline-show-hover nil
            lsp-ui-sideline-show-code-actions nil
            lsp-ui-sideline-update-mode 'point)
      (setq lsp-ui-doc-enable nil
            lsp-ui-doc-position 'at-point
            lsp-ui-doc-max-width 125
            lsp-ui-doc-max-height 18)
      (keymap-set lsp-mode-map "C-c r d" #'lsp-ui-doc-toggle)
      (keymap-set lsp-mode-map "C-c r i" #'lsp-ui-doc-focus-frame)
      (keymap-set lsp-mode-map "C-c f s" #'lsp-ui-find-workspace-symbol))
  '';
}
