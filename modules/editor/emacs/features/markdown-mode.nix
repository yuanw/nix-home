{
  epkgs = epkgs: [
    epkgs.markdown-mode
  ];

  elisp = ''
    (use-package markdown-mode
      :mode ("\\.mdwn\\'"
             "\\.markdown\\'"
             "\\.md\\'"))
  '';
}
