{
  elisp = ''
    ;; Follow the macOS appearance without Apple Events.
    ;;
    ;; `auto-dark' can only ask macOS by AppleScript: its applescript branch uses
    ;; `ns-do-applescript' and its fallback shells out to osascript (what
    ;; `auto-dark-allow-osascript' enabled); which one it picks depends on
    ;; whether `window-system' is set yet.  Either way the request goes to System
    ;; Events, TCC attributes it to the daemon's store path, and each rebuild
    ;; left a stale kTCCServiceAppleEvents row plus a new prompt behind.
    ;; The `ns-system-appearance' hook from patches/system-appearance.patch is no
    ;; help: it is registered in applicationDidFinishLaunching:, and
    ;; (boundp 'ns-system-appearance) is nil under --fg-daemon.  `defaults' needs
    ;; no TCC grant; we still poll, as auto-dark did.
    (defvar my/system-appearance--dark nil
      "Last observed system appearance: non-nil when dark.")

    (defvar my/system-appearance--timer nil
      "Timer polling the system appearance, or nil when not running.")

    (defun my/system-appearance-dark-p ()
      "Return non-nil when macOS is in dark mode.
    `defaults' prints usage and fails when the arguments are wrong, and exits
    1 when AppleInterfaceStyle is absent (light mode), so require both a zero
    exit and the value itself.  The path is absolute because the launchd
    daemon has a minimal exec-path."
      (with-temp-buffer
        (and (eq 0 (process-file "/usr/bin/defaults" nil t nil
                                 "read" "-g" "AppleInterfaceStyle"))
             (string-prefix-p "Dark" (string-trim (buffer-string))))))

    (defun my/system-appearance-sync (&optional force)
      "Load the Ef theme matching the system appearance, when it changed.
    With FORCE, apply even if the appearance is unchanged.  Use
    `ef-themes-load-theme', not `ef-themes-select': since ef-themes 2.0 the
    latter is the interactive completer and takes no argument.  Pass it only
    the theme -- its optional second argument errors in this Emacs build, and
    `custom-safe-themes' is t anyway."
      (let ((dark (my/system-appearance-dark-p)))
        (when (or force (not (eq dark my/system-appearance--dark)))
          (setq my/system-appearance--dark dark)
          (require 'ef-themes)
          (ef-themes-load-theme (if dark 'ef-owl 'ef-spring)))))

    (when (eq system-type 'darwin)
      ;; One second before the first poll so the daemon is up, then the same
      ;; five second cadence `auto-dark-polling-interval-seconds' used.  Kept in
      ;; a variable so it can be cancelled, and re-entrant if this is evaluated
      ;; twice.
      (when my/system-appearance--timer
        (cancel-timer my/system-appearance--timer))
      (setq my/system-appearance--timer
            (run-at-time 1 5 #'my/system-appearance-sync)))
  '';
}
