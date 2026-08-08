{ lspStyle, pkgs, ... }:

{
  enable = lspStyle == "eglot";

  epkgs = epkgs: [
    epkgs.eglot-booster
    pkgs.emacs-lsp-booster
  ];

  elisp = ''
    (use-package eglot-booster
      :after eglot
      :config
      (eglot-booster-mode))
  '';
}
