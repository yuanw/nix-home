{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.tmux;
  tmuxMenuSeperator = "''";
  tat = pkgs.writeShellScriptBin "tat" (builtins.readFile ./tat);
  td = pkgs.writeShellScriptBin "td" (builtins.readFile ./ta);
  temacs = pkgs.writeShellScriptBin "temacs" ''(tmux has-session -t emacs && tmux switch-client -t emacs) || (tmux new-session -Ad -s emacs && tmux send-keys -t emacs "emacsclient -c -a 'emacs'" "C-m" )'';
  tkill = pkgs.writeShellScriptBin "tkill" "tmux list-sessions -F '#{?session_attached,,#{session_name}}' | sed '/^$/d' | fzf --reverse --header kill-sessions --preview 'tmux capture-pane -pt {}'  | xargs tmux kill-session -t";

  # tmux-which-key integration
  tmuxWhichKeyYaml = builtins.replaceStrings [ "__OPENSESSIONS_DIR__" ] [ cfg.opensessions.dataDir ] (
    builtins.readFile ./tmux-which-key.yaml
  );

  tmuxWhichKeyInit =
    pkgs.runCommand "tmux-which-key-init.tmux"
      {
        nativeBuildInputs = [ pkgs.python3 ];
      }
      ''
        ${pkgs.python3}/bin/python3 ${pkgs.tmuxPlugins.tmux-which-key}/share/tmux-plugins/tmux-which-key/plugin/build.py \
          ${pkgs.writeText "tmux-which-key-config.yaml" tmuxWhichKeyYaml} \
          $out
      '';
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
    whichKey = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable tmux-which-key plugin for keybinding menu";
      };
      prefixKey = mkOption {
        type = types.str;
        default = "Space";
        description = "Key to trigger which-key menu after prefix";
      };
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
        ]

        ;
      };

      # tmux-which-key config
      xdg.configFile."tmux/plugins/tmux-which-key/config.yaml" = mkIf cfg.whichKey.enable {
        text = tmuxWhichKeyYaml;
      };
      xdg.dataFile."tmux/plugins/tmux-which-key/init.tmux" = mkIf cfg.whichKey.enable {
        source = tmuxWhichKeyInit;
      };

      programs = {
        tmux = {
          aggressiveResize = true;
          baseIndex = 1;
          enable = true;
          terminal = "screen-256color";
          clock24 = true;
          plugins = with pkgs; [
            # tmuxPlugins.fzf-tmux-url
            # tmuxPlugins.prefix-highlight
          ];
          customPaneNavigationAndResize = true;
          escapeTime = 0;
          historyLimit = 50000;
          keyMode = "emacs";
          shortcut = "n";
          extraConfig = ''
            # Status bar styling
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
            set -g status-left "#[fg=colour232,bg=colour117] #{?client_prefix,#[fg=white],} #S #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]"
            set -g status-right "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] %Y-%m-%d  %H:%M #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour232,bg=colour117] #h "
            setw -g window-status-format "#[fg=colour60,bg=colour60] #I #[fg=colour60,bg=colour60] #W "
            setw -g window-status-current-format "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] #I #[fg=colour146,bg=colour60] #W #[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]"

            # General settings
            set -g mouse on
            set -g extended-keys on
            set -g extended-keys-format csi-u
            set-option -g renumber-windows on

            # Pane/window bindings
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



            # tmux-which-key
            ${lib.optionalString cfg.whichKey.enable ''
              set -g @tmux-which-key-xdg-enable 1
              set -g @tmux-which-key-disable-autobuild 1
              bind-key ${cfg.whichKey.prefixKey} run-shell "${pkgs.coreutils}/bin/cat $XDG_DATA_HOME/tmux/plugins/tmux-which-key/init.tmux"
            ''}

            # Session menu (keep at bottom)
            bind-key Tab display-menu -T "#[align=centre]Sessions" "Switch" . 'choose-session -Zw' Last l "switch-client -l" ${tmuxMenuSeperator} \
              "Open Main Workspace" m "display-popup -E \" td ${cfg.mainWorkspaceDir} \"" ${tmuxMenuSeperator} \
              "Kill Current Session" k "run-shell 'tmux switch-client -n \; tmux kill-session -t #{session_name}'"  "Kill Other Sessions" o "display-popup -E \"tkill \"" ${tmuxMenuSeperator} \
              Random r "run-shell 'tat random'" org a "run-shell 'tat org'" ${tmuxMenuSeperator} \
              Exit q detach"
          '';
        };
        zsh = {
          sessionVariables = {
            ZSH_TMUX_AUTOSTART = "false";
            ZSH_TMUX_CONFIG = "$XDG_CONFIG_HOME/tmux/tmux.conf";
          };

          initContent = mkAfter ''
            # Auto-start tmux only in Alacritty
            if [ -n "$ALACRITTY_WINDOW_ID" ] && [ -z "$TMUX" ]; then
              tmux attach 2>/dev/null || tmux new-session
              exit
            fi

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
