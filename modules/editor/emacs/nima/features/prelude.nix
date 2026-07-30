{ ... }:

{
  order = -1000;

  epkgs = epkgs: [
    epkgs.use-package
  ];

  elisp = ''
    (eval-when-compile
      (require 'use-package))
  '';
}
