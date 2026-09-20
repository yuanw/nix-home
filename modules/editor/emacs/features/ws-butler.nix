{
  epkgs = epkgs: [ epkgs.ws-butler ];
  elisp = ''
    (use-package ws-butler
      :hook ((prog-mode text-mode) . ws-butler-mode))
  '';
}
