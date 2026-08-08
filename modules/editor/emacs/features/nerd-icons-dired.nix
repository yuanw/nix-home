{
  epkgs = epkgs: [
    epkgs.nerd-icons-dired
  ];

  elisp = ''
    (use-package nerd-icons-dired
      :hook (dired-mode . nerd-icons-dired-mode))
  '';
}
