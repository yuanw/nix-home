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

  # Every command carries a `;;;###autoload' cookie, so the library stays off
  # the startup path; jump in with M-x rex-gt-tab-manager.
  requireEpkgs = false;
}
