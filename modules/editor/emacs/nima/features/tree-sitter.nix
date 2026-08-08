{ pkgs, ... }:

{
  epkgs = epkgs: [
    epkgs.tree-sitter
    epkgs.tree-sitter-langs
    epkgs.treesit-grammars.with-all-grammars
  ];

  elisp = ''
    (use-package tree-sitter
      :config
      (setq treesit-extra-load-path
            '((eval-when-compile (expand-file-name "grammars" user-emacs-directory))
              "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib"
              "${pkgs.callPackage ../../../../../packages/tree-sitter-moonbit.nix { }}/lib"))
      (global-tree-sitter-mode)
      (add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode)
      (setq major-mode-remap-alist
            '((yaml-mode . yaml-ts-mode)
              (bash-mode . bash-ts-mode)
              (js2-mode . js-ts-mode)
              (json-mode . json-ts-mode)
              (css-mode . css-ts-mode)
              (python-mode . python-ts-mode)
              (zig-mode . zig-ts-mode))))

    (use-package tree-sitter-langs
      :after tree-sitter
      :config
      (add-to-list 'tree-sitter-major-mode-language-alist '(markdown-mode . markdown)))
  '';
}
