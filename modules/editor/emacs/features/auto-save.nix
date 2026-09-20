{
  epkgs = epkgs: [
    epkgs.auto-save
  ];

  elisp = ''
    (use-package auto-save
      :config
      (auto-save-enable)
      (setq auto-save-silent t)
      ;; Disable auto-save for GPG buffers.
      (setq auto-save-disable-predicates
            '((lambda ()
                (string-suffix-p
                 "gpg"
                 (file-name-extension (buffer-name))
                 t)))))
  '';
}
