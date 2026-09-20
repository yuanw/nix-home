{
  elisp = ''
    (use-package saveplace
      :defer 1
      :config
      (setq save-place t)
      (setq save-place-file (locate-user-emacs-file "places"))
      (save-place-mode 1))
  '';
}
