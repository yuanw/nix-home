{config, lib, pkgs, inputs} :
let
   emacsPackage = config.home-manager.users.${config.my.username}.programs.emacs.finalPackage;
in
with lib; {
  config = mkIf config.modules.editor.emacs.enableService {
      launchd.user.agents.emacs.path = [
        config.environment.systemPath
        "${config.my.homeDirectory}/.nix-profile/bin"
      ];

      launchd.user.agents.emacs.serviceConfig = {
        KeepAlive = true;
        ProgramArguments = [
          "/bin/zsh"
          "-c"
          "{ osascript -e 'display notification \"Attempting to start Emacs...\" with title \"Emacs Launch\"'; /bin/wait4path ${emacsPackage}/bin/emacs && { ${emacsPackage}/bin/emacs --fg-daemon; if [ $? -eq 0 ]; then osascript -e 'display notification \"Emacs has started.\" with title \"Emacs Launch\"'; else osascript -e 'display notification \"Failed to start Emacs.\" with title \"Emacs Launch\"' >&2; fi; } } &> /tmp/emacs.log"
        ];
        StandardErrorPath = "/tmp/emacs.log";
        StandardOutPath = "/tmp/emacs.log";
      };
  };
}
