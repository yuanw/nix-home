{
  epkgs = epkgs: [ epkgs.easy-kill ];
  elisp = ''
    (use-package easy-kill
      :bind (([remap kill-ring-save] . easy-kill)
             ([remap mark-sexp] . easy-mark)))
  '';
}
