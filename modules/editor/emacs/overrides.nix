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
      inherit (self) melpaBuild pcre2el;
    }
  );
}
// (emacsGhostel.emacsOverrides self _super)
