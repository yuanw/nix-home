{
  epkgs = epkgs: [
    epkgs.browse-kill-ring
  ];

  elisp = ''
    (use-package browse-kill-ring
      :defer t
      :commands (browse-kill-ring))
  '';
}
