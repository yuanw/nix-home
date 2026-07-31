{ lib, ... }:

let
  up = import ../lib/use-package.nix { inherit lib; };
in
{
  epkgs = epkgs: [
    epkgs.cmake-mode
  ];

  elisp = up.mkUsePackage "cmake-mode" {
    mode = [
      ''"\\.cmake\\'"''
      ''"CMakeLists.txt\\'"''
    ];
  };
}
