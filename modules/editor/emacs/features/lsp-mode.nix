{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  epkgs = epkgs: [
    epkgs.lsp-mode
  ];

  elisp = ''
    (use-package lsp-mode
      :after flycheck
      :commands (lsp)
      :init
      (setq lsp-keymap-prefix "C-r l")
      :config
      (setq lsp-diagnostics-provider :flycheck
            lsp-eldoc-render-all nil
            lsp-enable-on-type-formatting nil
            lsp-enable-suggest-server-download nil
            lsp-headerline-breadcrumb-enable nil
            lsp-lens-enable t
            lsp-modeline-code-actions-enable nil
            lsp-modeline-diagnostics-enable nil
            lsp-modeline-workspace-status-enable nil)
      (keymap-set lsp-mode-map "C-c f r" #'lsp-find-references)
      (keymap-set lsp-mode-map "C-r a" #'lsp-execute-code-action)
      (keymap-set lsp-mode-map "C-r f" #'lsp-format-buffer)
      (keymap-set lsp-mode-map "C-r g" #'lsp-format-region)
      (keymap-set lsp-mode-map "C-r l" #'lsp-avy-lens)
      (keymap-set lsp-mode-map "C-r r" #'lsp-rename))
  '';
}
