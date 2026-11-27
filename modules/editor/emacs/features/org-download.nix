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
       :config
       (setq org-download-method 'directory)
       (setq-default org-download-image-dir (expand-file-name "org/denote/screenshots" (getenv "HOME")))
       (setq-default org-download-heading-lvl nil)
       (add-hook 'dired-mode-hook #'org-download-enable))
  '';
}
