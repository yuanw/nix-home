{
  epkgs = epkgs: [
    epkgs.terraform-mode
  ];

  elisp = ''
    (use-package terraform-mode
      :mode ("\\.tf\\(vars\\)?\\'")
      :config
      (setq terraform-indent-level 4)
      (defun my-terraform-mode-init ()
        (outline-minor-mode 1))
      (add-hook 'terraform-mode-hook #'my-terraform-mode-init))
  '';
}
