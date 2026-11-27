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
      (setq org-download-image-dir (expand-file-name "org/denote/screenshots" (getenv "HOME")))
      (setq org-download-heading-lvl nil)
      (add-hook 'dired-mode-hook #'org-download-enable))
  '';
}
