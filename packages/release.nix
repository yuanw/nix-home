{
  pkgs ? import <nixpkgs> { },
  ...
}:
{
  sketchybar-cpu-helper = pkgs.callPackage ./sketchybar-cpu-helper { };
  bandcamp-dl = pkgs.python3Packages.callPackage ./bandcamp { };
  choose-mac = pkgs.callPackage ./choose-mac.nix { };
  sf-symbols = pkgs.callPackage ./sf_symbols.nix { };
  font-hack-nerd-font = pkgs.callPackage ./font-hack-nerd-font.nix { };
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/emacs-packages.nix
  hurl-mode = pkgs.callPackage ./emacs/hurl-mode.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };
  claude-code-ide = pkgs.callPackage ./emacs/claudeg-code-ide {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };
  ultra-scroll = pkgs.callPackage ./emacs/ultra-scroll.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };
}
