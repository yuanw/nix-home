{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.terminal;
  tmuxMenuSeperator = "''";
  tat = pkgs.writeShellScriptBin "tat" (builtins.readFile ./tat);
  td = pkgs.writeShellScriptBin "td" (builtins.readFile ./ta);
  temacs = pkgs.writeShellScriptBin "temacs" ''
    (tmux has-session -t emacs && tmux switch-client -t emacs) || (tmux new-session -Ad -s emacs && tmux send-keys -t emacs "doom run" "C-m" )'';
  tkill = pkgs.writeShellScriptBin "tkill"
    "tmux list-sessions -F '#{?session_attached,,#{session_name}}' | sed '/^$/d' | fzf --reverse --header kill-sessions --preview 'tmux capture-pane -pt {}'  | xargs tmux kill-session -t";
in {

  options.modules.terminal = {
    enable = mkEnableOption "terminal";
    mainWorkspaceDir = mkOption {
      default = "$HOME/workspace";
      type = types.str;
      description = "directory for prefix+O to point to";
    };

    secondaryWorkspaceDir = mkOption {
      default = "$HOME/workspace";
      type = types.str;
      description = "secondary directory for prefix+O to point to";
    };
  };
  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home = {
        file."moonlander.pdf".source = ../../pictures/moonlander.pdf;
        packages = [
          tat
          td
          tkill
          temacs
          pkgs.jo

        ];
      };
      programs = {
        pet = {
          enable = true;
          snippets = [
            {
              description = "update a flake source";
              command = " nix flake lock --update-input <input=nixpkgs>";
              tag = [ "nix" ];
            }
            {
              description = "Clean up your system profile";
              command = "sudo nix-collect-garbage --delete-older-than <day=3>d";
              tag = [ "nix" ];
            }

            {
              description = "Clean up your system profile";
              command = "sudo nix-store --optimise";
              tag = [ "nix" ];
            }

            {
              description = "restart emacs user agent service in macos";
              command = "sudo launchctl kickstart -k gui/$UID/org.nixos.emacs";
              tag = [ "emacs" "macos" ];
            }
            {
              description = "check all services";
              command = "checkin my services";
              tag = [ "slack" ];
            }
            {
              description = "check all services";
              command = "checkin content-management-service";
              tag = [ "slack" ];
            }

          ];
        };
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
        zsh = {
          sessionVariables = {
            # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tmux#configuration-variables
            # automatically start tmux
            ZSH_TMUX_AUTOSTART = "true";
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
          oh-my-zsh = { plugins = [ "tmux" ]; };
        };
        tmux = {
          aggressiveResize = true;
          baseIndex = 1;
          enable = true;
          terminal = "screen-256color";
          clock24 = true;
          plugins = with pkgs;
            [
              # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/misc/tmux-plugins/default.nix#L269
              tmuxPlugins.fzf-tmux-url
            ];
          customPaneNavigationAndResize = true;
          escapeTime = 0;
          historyLimit = 50000;
          keyMode = "vi";
          shortcut = "Space";
          extraConfig = ''
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
              "Open Main Workspace" m "display-popup -E \" td ${cfg.mainWorkspaceDir} \"" "Open Sec Workspace" s "display-popup -E \" td ${cfg.secondaryWorkspaceDir} \""   ${tmuxMenuSeperator} \
              "Kill Current Session" k "run-shell 'tmux switch-client -n \; tmux kill-session -t #{session_name}'"  "Kill Other Sessions" o "display-popup -E \"tkill \"" ${tmuxMenuSeperator} \
              Random r "run-shell 'tat random'" Emacs e "run-shell 'temacs'" ${tmuxMenuSeperator} \
              Exit q detach"
          '';
        };
        zellij = { enable = true; };

        # https://github.com/alacritty/alacritty/blob/master/alacritty.yml#L1
        alacritty = {
          enable = true;
          settings = {
            font = {
              normal = {
                family = config.my.font;
                style = "Regular";
              };
              bold = {
                family = config.my.font;
                style = "Bold";
              };
              italic = {
                family = config.my.font;
                style = "Italic";
              };
              size = 18;
            };
            cursor.style = "Beam";
            key_bindings = [
              # { key = "Space";  mods= "Control";                 action= "ToggleViMode";            }
            ];
            window.padding = {
              x = 12;
              y = 0;
            };
            window.decorations = "none";
            # window.dynamic_padding = false;
            # https://github.com/eendroroy/alacritty-theme/blob/master/themes/palenight.yml
            colors = {
              # Default colors
              primary = {
                background = "0x292d3e";
                foreground = "0xd0d0d0";
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
                white = "0xd0d0d0";
              };
              # Bright colors
              bright = {
                black = "0x434758";
                red = "0xff8b92";
                green = "0xddffa7";
                yellow = "0xffe585";
                blue = "0x9cc4ff";
                magenta = "0xe1acff";
                cyan = "0xa3f7ff";
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
