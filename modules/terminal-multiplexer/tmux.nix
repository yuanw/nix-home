{ config, lib, pkgs, ... }:
let
  cfg = config.modules.tmux;
  tat = pkgs.writeShellScriptBin "tat" (builtins.readFile ./tat);
  td = pkgs.writeShellScriptBin "td" (builtins.readFile ./ta);
  temacs = pkgs.writeShellScriptBin "temacs" ''
    (tmux has-session -t emacs && tmux switch-client -t emacs) || (tmux new-session -Ad -s emacs && tmux send-keys -t emacs "emacsclient -c -a 'emacs'" "C-m" )'';
  tkill = pkgs.writeShellScriptBin "tkill"
    "tmux list-sessions -F '#{?session_attached,,#{session_name}}' | sed '/^$/d' | fzf --reverse --header kill-sessions --preview 'tmux capture-pane -pt {}'  | xargs tmux kill-session -t";
in
with lib; {
  options.modules.tmux = {
    enablg = mkOption {
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
    };
  };

}
