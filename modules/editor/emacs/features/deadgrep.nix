{ pkgs, ... }:

{
  epkgs = epkgs: [
    epkgs.deadgrep
    pkgs.ripgrep
  ];

  elisp = ''
    (use-package deadgrep
      :config
      (setq deadgrep-executable "${pkgs.ripgrep}/bin/rg"))
  '';
}
