{
  pkgs,
  lib,
  packagePath,
  emacsGhostel,
}:

self: _super:
{

  gptel = (
    pkgs.callPackage "${packagePath}/gptel.nix" {
      inherit (pkgs)
        fetchFromGitHub
        writeText
        unstableGitUpdater
        ;
      inherit lib;
      inherit (self)
        melpaBuild
        transient
        compat
        ;
    }
  );
  acp = (
    pkgs.callPackage "${packagePath}/acp.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild;
    }
  );
  shell-maker = (
    pkgs.callPackage "${packagePath}/shell-maker.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild;
    }
  );
  md-ts-mode = (
    pkgs.callPackage "${packagePath}/md-ts-mode.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild;
    }
  );
  markdown-table-wrap = (
    pkgs.callPackage "${packagePath}/markdown-table-wrap.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild;
    }
  );
  pi-coding-agent = (
    pkgs.callPackage "${packagePath}/pi-coding-agent.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self)
        melpaBuild
        transient
        md-ts-mode
        markdown-table-wrap
        ;
    }
  );
  agent-shell = (
    pkgs.callPackage "${packagePath}/agent-shell.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild shell-maker acp;
    }
  );
  agent-shell-knockknock = (
    pkgs.callPackage "${packagePath}/agent-shell-knockknock.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild agent-shell knockknock;
    }
  );
  agent-shell-manager = (
    pkgs.callPackage "${packagePath}/agent-shell-manager.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild agent-shell;
    }
  );
  whisper = (
    pkgs.callPackage "${packagePath}/whisper-el.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild;
    }
  );
  moonbit-mode = (
    pkgs.callPackage "${packagePath}/moonbit-mode.nix" {
      inherit (pkgs) fetchFromGitHub;
      inherit (self) trivialBuild;
    }
  );
  hel = (
    pkgs.callPackage "${packagePath}/hel.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self)
        melpaBuild
        pcre2el
        dash
        avy
        ultra-scroll
        ;
    }
  );
  hel-leader = (
    pkgs.callPackage "${packagePath}/hel-leader.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self)
        melpaBuild
        hel
        dash
        s
        ;
    }
  );
  hel-ghostel = (
    pkgs.callPackage "${packagePath}/hel-ghostel.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self)
        melpaBuild
        hel
        ghostel
        dash
        ;
    }
  );
  hel-collection = (
    pkgs.callPackage "${packagePath}/hel-collection.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self)
        melpaBuild
        hel
        dash
        ;
    }
  );
  ultra-scroll = (
    pkgs.callPackage "${packagePath}/ultra-scroll.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit lib;
      inherit (self) melpaBuild;
    }
  );
  ask-mode = (
    pkgs.callPackage "${packagePath}/ask-mode" {
      inherit (pkgs) haskellPackages;
      inherit (self) melpaBuild;
    }
  );
  auto-save = (
    pkgs.callPackage "${packagePath}/auto-save.nix" {
      inherit (pkgs) fetchFromGitHub;
      inherit (self) melpaBuild;
    }
  );
  gptel-quick = (
    pkgs.callPackage "${packagePath}/gptel-quick.nix" {
      inherit (pkgs) fetchFromGitHub;
      inherit (self)
        melpaBuild
        compat
        gptel
        ;
    }
  );
  knockknock = (
    pkgs.callPackage "${packagePath}/knockknock.nix" {
      inherit (pkgs) fetchFromGitHub;
      inherit (self)
        melpaBuild
        posframe
        nerd-icons
        ;
    }
  );
  consult-omni = (
    pkgs.callPackage "${packagePath}/consult-omni" {
      inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
      inherit lib;
      inherit (self)
        browser-hist
        consult
        consult-notes
        elfeed
        embark
        melpaBuild
        yequake
        ;
    }
  );
  hurl-mode = (
    pkgs.callPackage "${packagePath}/hurl-mode.nix" {
      inherit (pkgs) fetchFromGitHub writeText;
      inherit (self) melpaBuild;
    }
  );
  ob-racket = (
    pkgs.callPackage "${packagePath}/ob-racket.nix" {
      inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
      inherit (self) melpaBuild;
    }
  );
  prot-common = self.trivialBuild {
    pname = "prot-common";
    version = "0.0.1";
    src = ./packages/prot-common.el;
  };
  prot-modeline = self.trivialBuild {
    pname = "prot-modeline";
    version = "0.0.1";
    src = ./packages/prot-modeline.el;
    packageRequires = [ self.prot-common ];
  };
}
// (emacsGhostel.emacsOverrides self _super)
