{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.editors.emacs;
  emacsPackage = import ./nima.nix {
    inherit pkgs lib;
    myConfig = config.my;
    emacsConfig = cfg;
    nixConfig = config;
  };
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
      ProgramArguments = [
        "/bin/zsh"
        "-c"
        # No `osascript' here: `display notification' is an Apple Event to
        # System Events, which TCC attributes to the daemon's store path and so
        # re-prompts after every rebuild. The whole block already redirects to
        # /tmp/emacs.log, which is where these messages belong.
        "{ echo \"Attempting to start Emacs...\"; [[ -s \"${config.my.homeDirectory}/.wk/profile\" ]] && source \"${config.my.homeDirectory}/.wk/profile\"; /bin/wait4path ${emacsPackage}/bin/emacs && { ${emacsPackage}/bin/emacs --fg-daemon; if [ $? -eq 0 ]; then echo \"Emacs has started.\"; else echo \"Failed to start Emacs.\" >&2; fi; } } &> /tmp/emacs.log"
      ];
      StandardErrorPath = "/tmp/emacs-error.log";
      StandardOutPath = "/tmp/emacs.log";
    };
  };
}
