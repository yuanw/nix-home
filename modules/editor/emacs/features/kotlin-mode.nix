{
  epkgs = epkgs: [
    epkgs.kotlin-mode
  ];

  elisp = ''
    (use-package kotlin-mode
      :config
      (require 'kotlin-mode))
  '';
}
