{ lspStyle, ... }:

{
  enable = lspStyle == "lsp-mode";

  epkgs = epkgs: [
    epkgs.dap-mode
  ];

  elisp = ''
    (use-package dap-mode
      :after lsp-mode)
  '';
}
