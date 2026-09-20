{
  epkgs = epkgs: [
    epkgs.org-modern
  ];

  elisp = ''
    (use-package org-modern
      :after org
      :hook ((org-mode . org-modern-mode)
             (org-agenda-finalize . org-modern-agenda)))
  '';
}
