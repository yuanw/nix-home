{ ... }:

{
  epkgs = epkgs: [ epkgs.ghostel ];

  elisp = ''
    (use-package ghostel
      :commands (ghostel
                 ghostel-project
                 ghostel-next
                 ghostel-previous
                 ghostel-list-buffers
                 ghostel-project-list-buffers)
      :bind (("C-c t" . ghostel-project)
             ("C-c T" . ghostel))
      :custom
      (ghostel-shell-integration t)
      (ghostel-enable-osc52 t))
  '';
}
