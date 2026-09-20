{
  epkgs = epkgs: [
    epkgs.denote-journal
  ];

  elisp = ''
    (use-package denote-journal
      :commands (denote-journal-new-entry
                 denote-journal-new-or-existing-entry
                 denote-journal-link-or-create-entry)
      :hook (calendar-mode . denote-journal-calendar-mode)
      :bind (("C-c n t" . denote-journal-new-or-existing-entry))
      :config
      (setq denote-journal-directory
            (expand-file-name "journal" denote-directory))
      (setq denote-journal-keyword "journal"))
  '';
}
