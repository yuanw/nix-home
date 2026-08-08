{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  elisp = ''
    (use-package lsp-diagnostics
      :after lsp-mode)
  '';
}
