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
            tmuxPlugins.prefix-highlight
          ];
          customPaneNavigationAndResize = true;
          escapeTime = 0;
          historyLimit = 50000;
          keyMode = "vi";
          shortcut = "e";
          extraConfig = ''
            set -g mouse on
            set -g status-left '#{prefix_highlight}'
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
          oh-my-zsh = {
            plugins = [ "tmux" ];
          };
        };
      };
    };
  };

}
