{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
up.mkUsePackageFeature "markdown-mode" {
  mode = [
    ''"\\.mdwn\\'"''
    ''"\\.markdown\\'"''
    ''"\\.md\\'"''
  ];
}
