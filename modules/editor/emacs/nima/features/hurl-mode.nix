{ pkgs, ... }:

let
  packagePath = ../../../../../packages/emacs;
in
{
  epkgs = epkgs: [
    (pkgs.callPackage "${packagePath}/hurl-mode.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit (epkgs) melpaBuild;
    })
    pkgs.stable.hurl
  ];

  elisp = ''
    (use-package hurl-mode
      :config
      ;; `org-babel-load-languages' is defined by Org/Babel.  Declare it to
      ;; avoid a void-variable error if hurl-mode loads before Org/Babel, then
      ;; update it after Org/Babel is available.
      (defvar org-babel-load-languages)
      (with-eval-after-load 'ob
        (add-to-list 'org-babel-load-languages '(hurl . t))))
  '';
}
