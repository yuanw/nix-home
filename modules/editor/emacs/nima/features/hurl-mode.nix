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
      ;; `org-babel-load-languages' is defined by Org/Babel, so do not touch it
      ;; before Org has loaded.
      (with-eval-after-load 'ob-core
        (add-to-list 'org-babel-load-languages '(hurl . t))))
  '';
}
