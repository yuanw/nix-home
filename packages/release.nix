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

  auto-save = pkgs.callPackage ./emacs/auto-save.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub;
  };
  claude-code-ide = pkgs.callPackage ./emacs/claude-code-ide {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };

  consult-omni = pkgs.callPackage ./emacs/consult-omni {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };
  emacs-reader = pkgs.callPackage ./emacs/emacs-reader.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs)
      fetchFromGitea
      writableTmpDirAsHomeHook
      mupdf
      writeText
      ;

  };
  emacs-reveal = pkgs.callPackage ./emacs/emacs-reveal.nix {
    trivialBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitLab;
  };

  gptel = pkgs.callPackage ./emacs/gptel.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };
  hurl-mode = pkgs.callPackage ./emacs/hurl-mode.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };

  ob-racket = pkgs.callPackage ./emacs/ob-racket.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };

  ultra-scroll = pkgs.callPackage ./emacs/ultra-scroll.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };
}
