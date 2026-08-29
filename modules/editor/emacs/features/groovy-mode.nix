{
  epkgs = epkgs: [
    epkgs.groovy-mode
  ];

  elisp = ''
    (use-package groovy-mode
      :mode ("\\.gradle\\'"
             "\\.groovy\\'"
             "Jenkinsfile\\'"))
  '';
}
