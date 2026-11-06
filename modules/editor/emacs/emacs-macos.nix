{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.editors.emacs;
  # The Home Manager copy of Emacs.app keeps a stable path across rebuilds, so
  # macOS TCC grants (Full Disk Access, etc.) survive a generation change.
  emacsApp = "${config.my.homeDirectory}/Applications/Home Manager Apps/Emacs.app/Contents/MacOS/Emacs";
in
with lib;
{
  config = mkIf cfg.enableService {
    launchd.user.agents.emacs.path = [
      config.environment.systemPath
      "${config.my.homeDirectory}/.nix-profile/bin"
    ];

    launchd.user.agents.emacs.serviceConfig = {
      KeepAlive = true;
      # Judge the result by the server socket, not the exit code: KeepAlive can
      # restart this script while the old daemon still owns the socket, and that
      # duplicate `--fg-daemon` exits 1 with a perfectly live server. Log is
      # appended so a failed run's output survives the retry.
      ProgramArguments = [
        "/bin/zsh"
        "-c"
        "{ osascript -e 'display notification \"Attempting to start Emacs...\" with title \"Emacs Launch\"'; [[ -s \"${config.my.homeDirectory}/.wk/profile\" ]] && source \"${config.my.homeDirectory}/.wk/profile\"; sock=\"$(getconf DARWIN_USER_TEMP_DIR)emacs$(id -u)/server\"; /bin/wait4path \"${emacsApp}\" && { \"${emacsApp}\" --fg-daemon; if [ -S \"$sock\" ]; then osascript -e 'display notification \"Emacs has started.\" with title \"Emacs Launch\"'; else osascript -e 'display notification \"Failed to start Emacs.\" with title \"Emacs Launch\"' >&2; fi; } } &>> /tmp/emacs.log"
      ];
      StandardErrorPath = "/tmp/emacs-error.log";
      StandardOutPath = "/tmp/emacs.log";
    };
  };
}
