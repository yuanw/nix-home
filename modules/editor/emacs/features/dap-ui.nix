{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  elisp = ''
    (use-package dap-ui
      :hook (dap-mode . dap-ui-mode))
  '';
}
