{
  epkgs = epkgs: [
    epkgs.denote
  ];

  elisp = ''
    (use-package denote
      :after org
      :bind (("C-c n o" . denote-open-or-create))
      :config
      (setq denote-directory "~/org/denote/")
      (setq denote-templates
            `((report . "* Some heading\n\n* Another heading")
              (journal . ,(concat "* Tasks todo"
                                  "\n\n"
                                  "* TIL"
                                  "\n\n"))))
      (denote-rename-buffer-mode)
      (with-eval-after-load 'org-capture
        (push '("n" "New note (With Denote)" plain (file denote-last-path) denote-org-capture
                :no-save t :immediate-finish nil :kill-buffer t :jump-to-captured t)
              org-capture-templates)
        (push '("d" "daily note" item (function denote-journal-new-or-existing-entry) "- %U %?" :prepend t)
              org-capture-templates)))
  '';
}
