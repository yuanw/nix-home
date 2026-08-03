{ pkgs, ... }:

{
  epkgs = _epkgs: [
    pkgs.coreutils
  ];

  elisp = ''
    (use-package dired
      :commands (dired dired-jump)
      :config
      (put 'dired-find-alternate-file 'disabled nil)
      ;; macOS /bin/ls does not support --group-directories-first;
      ;; use GNU coreutils gls which does.
      (when (eq system-type 'darwin)
        (setq insert-directory-program "${pkgs.coreutils}/bin/gls")
        (setq dired-use-ls-dired t))
      ;; Be smart about choosing file targets.
      (setq dired-dwim-target t)
      (setq dired-auto-revert-buffer t)
      ;; Use the system trash can.
      (setq delete-by-moving-to-trash t)
      (setq dired-listing-switches "-alvh --group-directories-first"))
  '';
}
