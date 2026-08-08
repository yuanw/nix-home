{ pkgs, ... }:

{
  epkgs = _epkgs: [
    pkgs.google-java-format
  ];

  elisp = ''
    (use-package java-mode
      :no-require t
      :mode "\\.java\\'"
      :preface
      (defun my/format-java ()
        (interactive)
        (call-process "${pkgs.google-java-format}/bin/google-java-format" nil nil nil
                      "-r" (expand-file-name buffer-file-name))
        (revert-buffer-quick))
      :config
      (keymap-set java-mode-map "C-c C-f" #'my/format-java))
  '';
}
