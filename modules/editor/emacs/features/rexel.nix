{ pkgs, ... }:

# Rex in Emacs.  Where `ghostel' is a local terminal, `rexel' is a terminal of a
# Rex session on a Rex server: the shells keep running when Emacs goes away, and
# Rex.app can be attached to the same session.  The server itself and its key
# bindings come from modules/terminal-multiplexer/rex.nix, which also puts `rex'
# on PATH; this feature is the Emacs half, and without a server its commands
# only report that.
#
# Neither package is in nixpkgs (rexel is not on MELPA yet), so both are built
# from the one upstream checkout, each from its own directory of it.  They need
# ghostel 0.66.0, which is why that version is pinned in ../ghostel.nix.
{
  overlay = final: _prev: {
    rexel = final.melpaBuild {
      pname = "rexel";
      version = "0.6.0";
      src = pkgs.fetchFromGitHub {
        owner = "dakra";
        repo = "rexel";
        tag = "v0.6.0";
        hash = "sha256-6fI5P/QcQ1ZCk1+u+Zp40OgTWPXQC6NzHnNpONi6igk=";
      };
      # The Elisp is in lisp/; extensions/consult-rexel holds the pickers.
      files = ''("lisp/*.el")'';
      recipe = pkgs.writeText "recipe" ''
        (rexel
         :repo "dakra/rexel"
         :fetcher github
         :files ("lisp/*.el"))
      '';
      packageRequires = [
        final.ghostel
        final.websocket
      ];
      # Upstream byte-compiles with warnings as errors, so a ghostel that drifts
      # out from under it fails the build rather than Emacs at runtime.
      turnCompilationWarningToError = true;
    };

    consult-rexel = final.melpaBuild {
      pname = "consult-rexel";
      version = "0.6.0";
      src = final.rexel.src;
      files = ''("extensions/consult-rexel/*.el")'';
      recipe = pkgs.writeText "recipe" ''
        (consult-rexel
         :repo "dakra/rexel"
         :fetcher github
         :files ("extensions/consult-rexel/*.el"))
      '';
      # Embark and consult-ghostel are picked up when present, not required.
      packageRequires = [
        final.consult
        final.ghostel
        final.rexel
      ];
      turnCompilationWarningToError = true;
    };
  };

  epkgs = epkgs: [
    epkgs.rexel
    epkgs.consult-rexel
  ];

  # The interactive commands upstream autoloads cover `M-x rexel' and friends;
  # `rexel-prefix-key' (C-c C-r) prefixes the split, tab, upload and port
  # commands once in a block buffer.  No `:after' on consult-rexel: its mode is
  # what adds Rex blocks to `consult-buffer', so it has to be on at startup,
  # which means the hook -- and with it the load of rexel -- cannot hide behind
  # rexel being loaded.
  elisp = ''
    (use-package rexel
      :bind (("C-c r" . rexel-project)
             ("C-c R" . rexel))
      :custom
      ;; Show a session's focused block instead of asking what to show.
      (rexel-attach-display 'focused))

    (use-package consult-rexel
      :hook (after-init . consult-rexel-mode))
  '';
}
