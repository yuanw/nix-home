{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  elisp = ''
    (use-package lsp-lens
      :after lsp-mode
      :commands (lsp-lens--enable))
  '';
}
