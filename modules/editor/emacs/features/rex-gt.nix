{ ... }:

{
  # My own package, built from the Elisp in ../packages.  It shells out to the
  # `rex' CLI, which modules/terminal-multiplexer/rex.nix puts on PATH.
  overlay = final: _prev: {
    rex-gt = final.trivialBuild {
      pname = "rex-gt";
      version = "0.1.0";
      src = ../packages/rex-gt.el;
      # The build should fail on the warnings that mean a bug, not just print
      # them into a log nobody reads.
      turnCompilationWarningToError = true;
    };
  };

  epkgs = epkgs: [ epkgs.rex-gt ];

  # trivialBuild ships no loaddefs, so `:commands' is what makes M-x see them.
  elisp = ''
    (use-package rex-gt
      :commands
      (rex-gt-tab-manager
       rex-gt-go
       rex-gt-capture
       rex-gt-new-tab
       rex-gt-focus-app
       rex-gt-open-pane-directory))
  '';

  # The commands above autoload it, and Rex's key calls it through emacsclient.
  requireEpkgs = false;
}
