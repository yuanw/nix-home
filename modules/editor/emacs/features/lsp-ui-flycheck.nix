{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  elisp = ''
    (use-package lsp-ui-flycheck
      :after (flycheck lsp-ui))
  '';
}
