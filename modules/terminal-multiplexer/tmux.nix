{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.modules.tmux;
  tmuxMenuSeperator = "''";
  tat = pkgs.writeShellScriptBin "tat" (builtins.readFile ./tat);
  td = pkgs.writeShellScriptBin "td" (builtins.readFile ./ta);
  temacs = pkgs.writeShellScriptBin "temacs" ''(tmux has-session -t emacs && tmux switch-client -t emacs) || (tmux new-session -Ad -s emacs && tmux send-keys -t emacs "emacsclient -c -a 'emacs'" "C-m" )'';
  tkill = pkgs.writeShellScriptBin "tkill" "tmux list-sessions -F '#{?session_attached,,#{session_name}}' | sed '/^$/d' | fzf --reverse --header kill-sessions --preview 'tmux capture-pane -pt {}'  | xargs tmux kill-session -t";
in
with lib;
{
  options.modules.tmux = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    mainWorkspaceDir = mkOption {
      default = "$HOME/workspace";
      type = types.str;
      description = "directory for prefix+m to point to";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home = {
        file."reiryoku.svg".source = ../../pictures/reiryoku.svg;
        packages = [
          tat
          td
          tkill
          temacs

        ];
      };
      programs = {
        tmux = {
          aggressiveResize = true;
          baseIndex = 1;
          enable = true;
          terminal = "screen-256color";
          clock24 = true;
          plugins = with pkgs; [
            # bind is u
            # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/misc/tmux-plugins/default.nix#L269
            tmuxPlugins.fzf-tmux-url
          ];
          customPaneNavigationAndResize = true;
          escapeTime = 0;
          historyLimit = 50000;
          keyMode = "vi";
          shortcut = "Space";
          extraConfig = ''
            #set-option -g default-shell /bin/zsh
            #set -g default-command /bin/zsh
            set -g status-justify "left"
            set -g status "on"
            set -g status-left-style none
            set -g message-command-style "fg=colour146,bg=colour60"
            set -g status-right-style "none"
            set -g pane-active-border-style "fg=colour117"
            set -g status-style bg=colour60
            set -g message-style "fg=colour146,bg=colour60"
            set -g pane-border-style "fg=colour60"
            set -g status-right-length "100"
            set -g status-left-length "100"
            setw -g window-status-activity-style "none"
            setw -g window-status-separator ""
            setw -g window-status-style "none,fg=colour60,bg=colour60"
            set -g status-left "#[fg=colour232,bg=colour117] #{?client_prefix,#[fg=white],} #S #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]"
            set -g status-right "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] %Y-%m-%d  %H:%M #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour232,bg=colour117] #h "
            setw -g window-status-format "#[fg=colour60,bg=colour60] #I #[fg=colour60,bg=colour60] #W "
            setw -g window-status-current-format "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] #I #[fg=colour146,bg=colour60] #W #[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]"
            set -g mouse on
            bind v split-window -h -c '#{pane_current_path}'
            bind s split-window -v -c '#{pane_current_path}'

            bind c new-window -c '#{pane_current_path}'

            bind-key R source-file $XDG_CONFIG_HOME/tmux/tmux.conf \; display-message "$XDG_CONFIG_HOME/tmux/tmux.conf reloaded"

            bind L switch-client -l
            bind J display-popup -E "\
                 tmux list-panes -a -F '#{?session_attached,,#S:#I.#P}' |\
                 sed '/^$/d' |\
                 fzf --reverse --header join-pane --preview 'tmux capture-pane -pt {}'  |\
                 xargs tmux join-pane -v -s"
            set-option -g renumber-windows on

            # keep this at the bottom
            bind-key Tab display-menu -T "#[align=centre]Sessions" "Switch" . 'choose-session -Zw' Last l "switch-client -l" ${tmuxMenuSeperator} \
              "Open Main Workspace" m "display-popup -E \" td ${cfg.mainWorkspaceDir} \"" ${tmuxMenuSeperator} \
              "Kill Current Session" k "run-shell 'tmux switch-client -n \; tmux kill-session -t #{session_name}'"  "Kill Other Sessions" o "display-popup -E \"tkill \"" ${tmuxMenuSeperator} \
              Random r "run-shell 'tat random'" Ollama a "run-shell 'tat ollama'" ${tmuxMenuSeperator} \
              Exit q detach"
          '';
        };
        zsh = {
          sessionVariables = {
            # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tmux#configuration-variables
            # automatically start tmux
            #ZSH_TMUX_AUTOSTART = "true";
            ZSH_TMUX_CONFIG = "$XDG_CONFIG_HOME/tmux/tmux.conf";
          };
          shellAliases = {
            # tkill =
            #   "tmux list-sessions -F '#{?session_attached,,#{session_name}}' | sed '/^$/d' | fzf --reverse --header kill-session --preview 'tmux capture-pane -pt {}'  | xargs tmux kill-session -t";

            # tkill =
            #   "for s in $(tmux list-sessions | awk '{print $1}' | rg ':' -r '' | fzf); do tmux kill-session -t $s; done;";
          };
          initExtra = mkAfter ''
            function zt {
               z $1 && tat
            }
          '';
          oh-my-zsh = {
            plugins = [ "tmux" ];
          };
        };
      };
    };
  };

}
