{ pkgs, ... }:

{
  epkgs = epkgs: [
    epkgs.flycheck
    pkgs.vale
  ];

  elisp = ''
    (use-package flycheck
      :config
      (global-flycheck-mode)
      (flycheck-define-checker vale
        "A checker for prose"
        :command ("${pkgs.vale}/bin/vale" "--output" "line" source)
        :standard-input nil
        :error-patterns
        ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
        :modes (markdown-mode gfm-mode org-mode text-mode))
      (add-to-list 'flycheck-checkers 'vale 'append))
  '';
}
