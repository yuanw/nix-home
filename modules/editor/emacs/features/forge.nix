{
  epkgs = epkgs: [
    epkgs.forge
  ];

  elisp = ''
    (use-package forge
      :after magit
      :config
      (setq forge-add-pullreq-refspec 'ask)
      (when-let ((gh (executable-find "gh")))
        (let ((token (string-trim (shell-command-to-string (format "%s auth token" gh)))))
          (when (and token (not (string-empty-p token)))
            (setenv "GH_TOKEN" token)))))
  '';
}
