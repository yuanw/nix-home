{
  epkgs = epkgs: [ epkgs.git-link ];
  elisp = ''
    (use-package git-link
      :commands (git-link git-link-commit git-link-homepage))
  '';
}
