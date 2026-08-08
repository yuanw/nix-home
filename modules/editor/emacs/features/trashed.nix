{
  epkgs = epkgs: [
    epkgs.trashed
  ];

  elisp = ''
    (use-package trashed
      :commands (trashed)
      :config
      (setq trashed-action-confirmer 'y-or-n-p)
      (setq trashed-use-header-line t)
      (setq trashed-sort-key '("Date deleted" . t))
      (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))
  '';
}
