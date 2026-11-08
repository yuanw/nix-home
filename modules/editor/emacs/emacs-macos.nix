{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.editors.emacs;
  # The Home Manager copy of Emacs.app keeps a stable path across rebuilds, so
  # macOS TCC grants (Full Disk Access, etc.) survive a generation change.
  emacsApp = "${config.my.homeDirectory}/Applications/Home Manager Apps/Emacs.app/Contents/MacOS/Emacs";
  # The daemon is configured and byte-compiled into this package, so its store
  # path is what says "the config changed".  Not used to launch Emacs.
  emacsPackage = import ./nima.nix {
    inherit pkgs lib;
    myConfig = config.my;
    emacsConfig = cfg;
    nixConfig = config;
  };
  # alerter instead of `osascript -e 'display notification'`: it posts as a real
  # sender app (--sender, default com.apple.Terminal) rather than as osascript,
  # and --timeout bounds the wait -- alerter otherwise blocks until the alert is
  # clicked or closed. --group makes a repeat replace the live one, which matters
  # because KeepAlive re-runs this script.
  alerter = "${pkgs.alerter}/bin/alerter";
in
with lib;
{
  config = mkIf cfg.enableService {
    launchd.user.agents.emacs.path = [
      config.environment.systemPath
      "${config.my.homeDirectory}/.nix-profile/bin"
    ];

    # nix-darwin reloads a user agent only when its plist differs from the one
    # in ~/Library/LaunchAgents (see nix-darwin modules/system/launchd.nix).
    # Since the launch path above is stable, the plist used to be identical
    # across rebuilds and the daemon kept running the old config forever; this
    # variable puts the Emacs build back into the plist so a switch restarts it.
    launchd.user.agents.emacs.environment.EMACS_BUILD = "${emacsPackage}";

    launchd.user.agents.emacs.serviceConfig = {
      KeepAlive = true;
      # Judge the result by the server socket, not the exit code: KeepAlive can
      # restart this script while the old daemon still owns the socket, and that
      # duplicate `--fg-daemon` exits 1 with a perfectly live server. Log is
      # appended so a failed run's output survives the retry.
      ProgramArguments = [
        "/bin/zsh"
        "-c"
        ''
          notify() { ${alerter} --title "Emacs Launch" --group emacs-launch --timeout 5 --message "$1"; }
          {
            notify "Attempting to start Emacs..."
            [[ -s "${config.my.homeDirectory}/.wk/profile" ]] && source "${config.my.homeDirectory}/.wk/profile"
            sock="$(getconf DARWIN_USER_TEMP_DIR)emacs$(id -u)/server"
            /bin/wait4path "${emacsApp}" && {
              "${emacsApp}" --fg-daemon
              if [ -S "$sock" ]; then
                notify "Emacs has started."
              else
                notify "Failed to start Emacs." >&2
              fi
            }
          } &>> /tmp/emacs.log
        ''
      ];
      StandardErrorPath = "/tmp/emacs-error.log";
      StandardOutPath = "/tmp/emacs.log";
    };
  };
}
