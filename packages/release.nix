{
  pkgs ? import <nixpkgs> { },
  ...
}:
rec {
  sketchybar-cpu-helper = pkgs.callPackage ./sketchybar-cpu-helper { };
  bandcamp-dl = pkgs.python3Packages.callPackage ./bandcamp { };
  choose-mac = pkgs.callPackage ./choose-mac.nix { };
  sf-symbols = pkgs.callPackage ./sf_symbols.nix { };
  font-hack-nerd-font = pkgs.callPackage ./font-hack-nerd-font.nix { };
  claude-code-acp = pkgs.callPackage ./claude-code-acp.nix { };
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
  gptel-agent = pkgs.callPackage ./emacs/gptel-agent.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };
  gptel-quick = pkgs.callPackage ./emacs/gptel-quick.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub;
  };
  hurl-mode = pkgs.callPackage ./emacs/hurl-mode.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };

  knockknock = pkgs.callPackage ./emacs/knockknock.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub;
  };

  lean4-mode = pkgs.callPackage ./emacs/lean4-mode.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText;
  };

  ob-gptel = pkgs.callPackage ./emacs/ob-gptel.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub;
  };

  ob-racket = pkgs.callPackage ./emacs/ob-racket.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText;
  };

  thrift-mode = pkgs.callPackage ./emacs/thrift-mode.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText;
  };

  ultra-scroll = pkgs.callPackage ./emacs/ultra-scroll.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
  };

  home-row-expreg = pkgs.callPackage ./emacs/expreg.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText;
  };

  acp = pkgs.callPackage ./emacs/acp.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText;
  };

  shell-maker = pkgs.callPackage ./emacs/shell-maker.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText;
  };

  agent-shell = pkgs.callPackage ./emacs/agent-shell.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub writeText;
    inherit shell-maker acp;
  };

  magit-ai = pkgs.callPackage ./emacs/magit-ai.nix {
    melpaBuild = pkgs.stdenv.mkDerivation;
    inherit (pkgs) fetchFromGitHub;
  };

  cozempic = pkgs.python3Packages.callPackage ./cozempic { };
  chroma-mcp = pkgs.python313Packages.callPackage ./chrome-mcp.nix { };
  aws-iam-authenticator = pkgs.callPackage ./aws-iam-authenticator.nix { };
  caveman = (pkgs.callPackage ./claude-plugins { }).caveman;
  claude-mem = (pkgs.callPackage ./claude-plugins { }).claude-mem;
  emacs-skills = (pkgs.callPackage ./claude-plugins { }).emacs-skills;
  humanizer = (pkgs.callPackage ./claude-plugins { }).humanizer;
}
