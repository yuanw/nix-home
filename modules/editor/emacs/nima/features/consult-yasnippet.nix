{
  epkgs = epkgs: [ epkgs.consult-yasnippet ];
  elisp = ''
    (use-package consult-yasnippet
      :after (consult yasnippet)
      :commands (consult-yasnippet))
  '';
}
