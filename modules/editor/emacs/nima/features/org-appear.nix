{
  epkgs = epkgs: [
    epkgs.org-appear
  ];

  elisp = ''
    (use-package org-appear
      :after org
      :hook (org-mode . org-appear-mode))
  '';
}
