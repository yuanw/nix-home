{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.markdown-mode
  ];

  elisp = up.mkUsePackage "markdown-mode" {
    mode = [
      ''"\\.mdwn\\'"''
      ''"\\.markdown\\'"''
      ''"\\.md\\'"''
    ];
  };
}
