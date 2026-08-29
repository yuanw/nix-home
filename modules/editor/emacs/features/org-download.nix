{ pkgs, ... }:

{
  epkgs =
    epkgs:
    [
      epkgs.org-download
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
      pkgs.pngpaste
    ];

  elisp = ''
    (use-package org-download
      :after org
      :commands (org-download-yank org-download-clipboard)
      :config
      (setq org-download-method 'directory)
      (add-hook 'dired-mode-hook #'org-download-enable))
  '';
}
