{ config, lib, pkgs, localConfig, ... }:

with lib;
let cfg = config.modules.terminal;
in {

  options.modules.terminal = { enable = mkEnableOption "terminal"; };
  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username} = {
      programs = {
        starship = {
          enable = true;
          enableZshIntegration = true;
          # https://starship.rs/config/#prompt
          settings = {
            aws = { disabled = true; };
            gcloud = { disabled = true; };
            git_status = {
              ahead = "⇡($count)";
              diverged = "⇕⇡($ahead_count)⇣($behind_count)";
              behind = "⇣($count)";
              modified = "!($count)";
              staged = "[++($count)](green)";
            };
          };
        };
        tmux = {
          aggressiveResize = true;
          baseIndex = 1;
          enable = true;
          terminal = "screen-256color";
          clock24 = true;
          customPaneNavigationAndResize = true;
          escapeTime = 0;
          historyLimit = 50000;
          keyMode = "vi";
          shortcut = "Space";
          extraConfig = ''
            bind v split-window -h -c '#{pane_current_path}'
            bind s split-window -v -c '#{pane_current_path}'

            bind c new-window -c '#{pane_current_path}'
            bind S choose-session -Zw

            bind-key R source-file $XDG_CONFIG_HOME/tmux/tmux.conf \; display-message "~/.tmux.conf reloaded"
            bind C-j new-window -n "session-switcher" "tmux list-sessions | sed -E 's/:.*$//' | grep -v \"^$(tmux display-message -p '#S')\$\" | fzf --reverse | xargs tmux switch-client -t"

            set-option -g renumber-windows on
            set -g status-justify "left"
            set -g status "on"
            set -g status-left-style "none"
            set -g message-command-style "fg=colour146,bg=colour60"
            set -g status-right-style "none"
            set -g pane-active-border-style "fg=colour117"
            set -g status-style "none,bg=colour60"
            set -g message-style "fg=colour146,bg=colour60"
            set -g pane-border-style "fg=colour60"
            set -g status-right-length "100"
            set -g status-left-length "100"
            setw -g window-status-activity-style "none"
            setw -g window-status-separator ""
            setw -g window-status-style "none,fg=colour60,bg=colour60"
            set -g status-left "#[fg=colour232,bg=colour117] #{?client_prefix,#[bg=colour2],} #S #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]"
            set -g status-right "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] %Y-%m-%d  %H:%M #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour232,bg=colour117] #h "
            setw -g window-status-format "#[fg=colour60,bg=colour60] #I #[fg=colour60,bg=colour60] #W "
            setw -g window-status-current-format "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] #I #[fg=colour146,bg=colour60] #W #[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]"
          '';
        };

        alacritty = {
          enable = true;
          settings = {
            font = {
              normal = {
                family = "PragmataPro";
                style = "Regular";
              };
              bold = {
                family = "PragmataPro";
                style = "Bold";
              };
              italic = {
                family = "PragmataPro";
                style = "Italic";
              };
              size = 18;
            };
            cursor.style = "Beam";
            key_bindings = [
              {
                key = "V";
                mods = "Control|Shift";
                action = "Paste";
              }
              {
                key = "C";
                mods = "Control|Shift";
                action = "Copy";
              }
              {
                key = "Up";
                mods = "Control|Shift";
                action = "ScrollPageUp";
              }
              {
                key = "Down";
                mods = "Control|Shift";
                action = "ScrollPageDown";
              }
            ];
            window.padding = {
              x = 12;
              y = 0;
            };
            window.decorations = "none";
            window.dynamic_padding = false;
            # background_opacity = 0.8;
            # base16-material-palenight-256
            # https://github.com/aarowill/base16-alacritty/blob/master/colors/base16-material-palenight-256.yml
            colors = {
              # Default colors
              primary = {
                background = "0x292d3e";
                foreground = "0x959dcb";
              };
              cursor = {
                text = "0x202331";
                cursor = "0xc792ea";
              };
              # Normal colors
              normal = {
                black = "0x292d3e";
                red = "0xf07178";
                green = "0xc3e88d";
                yellow = "0xffcb6b";
                blue = "0x82aaff";
                magenta = "0xc792ea";
                cyan = "0x89ddff";
                white = "0x959dcb";
              };
              # Bright colors
              bright = {
                black = "0x676e95";
                red = "0xf07178";
                green = "0xc3e88d";
                yellow = "0xffcb6b";
                blue = "0x82aaff";
                magenta = "0xc792ea";
                cyan = "0x89ddff";
                white = "0xffffff";
              };
              indexed_colors = [
                {
                  index = 16;
                  color = "0xf78c6c";
                }
                {
                  index = 17;
                  color = "0xff5370";
                }
                {
                  index = 18;
                  color = "0x444267";
                }
                {
                  index = 19;
                  color = "0x32374d";
                }
                {
                  index = 20;
                  color = "0x8796b0";
                }
                {
                  index = 21;
                  color = "0x959dcb";
                }
              ];
            };
          };
        };
      };
    };
  };
}
