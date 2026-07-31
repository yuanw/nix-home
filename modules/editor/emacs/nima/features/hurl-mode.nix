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
      (add-to-list 'org-babel-load-languages '(hurl . t)))
  '';
}
