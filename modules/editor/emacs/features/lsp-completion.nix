{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  elisp = ''
    (use-package lsp-completion
      :after lsp-mode
      :config
      (setq lsp-completion-enable-additional-text-edit nil))
  '';
}
