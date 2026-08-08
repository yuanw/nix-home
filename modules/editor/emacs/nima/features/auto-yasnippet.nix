{
  epkgs = epkgs: [ epkgs.auto-yasnippet ];
  elisp = ''
    (use-package auto-yasnippet
      :commands (aya-create aya-expand))
  '';
}
