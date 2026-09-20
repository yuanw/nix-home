{
  epkgs = epkgs: [ epkgs.org-review ];
  elisp = ''
    (use-package org-review)
  '';
}
