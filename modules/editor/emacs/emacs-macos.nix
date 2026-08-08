{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.editors.emacs;
  emacsPatched = cfg.pkg.overrideAttrs (prev: {
    patches = [
      ./patches/system-appearance.patch
      ./patches/round-undecorated-frame.patch
    ]
    ++ prev.patches;
  });
  emacsPackage = import ./nima {
    inherit pkgs;
    emacsPackage = emacsPatched;
    monoFont = config.my.monoFont;
    font = config.my.font;
    homeDirectory = config.my.homeDirectory;
    workspaceDirectory = config.my.workspaceDirectory;
    lspStyle = cfg.lspStyle;
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
        "{ osascript -e 'display notification \"Attempting to start Emacs...\" with title \"Emacs Launch\"'; [[ -s \"${config.my.homeDirectory}/.wk/profile\" ]] && source \"${config.my.homeDirectory}/.wk/profile\"; /bin/wait4path ${emacsPackage}/bin/emacs && { ${emacsPackage}/bin/emacs --fg-daemon; if [ $? -eq 0 ]; then osascript -e 'display notification \"Emacs has started.\" with title \"Emacs Launch\"'; else osascript -e 'display notification \"Failed to start Emacs.\" with title \"Emacs Launch\"' >&2; fi; } } &> /tmp/emacs.log"
      ];
      StandardErrorPath = "/tmp/emacs-error.log";
      StandardOutPath = "/tmp/emacs.log";
    };
  };
}
