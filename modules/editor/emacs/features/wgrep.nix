{
  epkgs = epkgs: [ epkgs.wgrep ];
  elisp = ''
    (use-package wgrep
      :commands (wgrep-change-to-wgrep-mode))
  '';
}
