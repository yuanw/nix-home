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

  # Opensessions runtime paths
  opensessionsSource = "${pkgs.opensessions}/share/opensessions";
  opensessionsRuntimeRoot = "${config.my.homeDirectory}/.local/share/opensessions";
  opensessionsCheckout = "${opensessionsRuntimeRoot}/${pkgs.opensessions.version}";
  opensessionsCurrent = "${opensessionsRuntimeRoot}/current";

  opensessionsConfigJson = builtins.toJSON {
    mux = "tmux";
    plugins = [ ];
    inherit (cfg.opensessions) theme;
    sidebarWidth = cfg.opensessions.width;
    inherit (cfg.opensessions) sidebarPosition;
    inherit (cfg.opensessions) showWindowDetails;
  };

  # Opensessions loader script
  opensessionsLoader = pkgs.writeShellScript "opensessions-loader.sh" ''
    #!/usr/bin/env sh
    set -eu

    OPENSESSIONS_DIR="${opensessionsCurrent}"
    PLUGIN_ENTRY="$OPENSESSIONS_DIR/opensessions.tmux"

    if [ ! -f "$PLUGIN_ENTRY" ]; then
      tmux display-message "opensessions: missing $PLUGIN_ENTRY"
      exit 0
    fi

    # Ensure tmux server PATH includes Nix-provided runtime deps for plugin scripts.
    PATH_PREFIX="${pkgs.curl}/bin:${pkgs.fzf}/bin:${pkgs.bun}/bin"
    CURRENT_PATH="$(tmux show-environment -g PATH 2>/dev/null | cut -d= -f2- || true)"

    if [ -n "$PATH_PREFIX" ]; then
      BASE_PATH="''${CURRENT_PATH:-$PATH}"
      case ":$BASE_PATH:" in
        *":$PATH_PREFIX:"*) ;;
        *) tmux set-environment -g PATH "$PATH_PREFIX:$BASE_PATH" ;;
      esac
    fi

    tmux set-environment -g OPENSESSIONS_DIR "$OPENSESSIONS_DIR"
    tmux set-environment -g BUN_PATH "${pkgs.bun}/bin/bun"
    tmux set-environment -g OPENSESSIONS_HOST "${cfg.opensessions.host}"
    tmux set-environment -g OPENSESSIONS_PORT "${toString cfg.opensessions.port}"

    exec sh "$PLUGIN_ENTRY"
  '';

  # Opensessions restart script
  opensessionsRestart = pkgs.writeShellScript "restart-opensessions.sh" ''
    #!/usr/bin/env sh
    set -eu

    OPENSESSIONS_DIR="${opensessionsCurrent}"
    SCRIPTS_DIR="$OPENSESSIONS_DIR/integrations/tmux-plugin/scripts"
    HOST="${cfg.opensessions.host}"
    PORT="${toString cfg.opensessions.port}"
    PID_FILE="/tmp/opensessions.pid"

    if [ ! -d "$SCRIPTS_DIR" ]; then
      tmux display-message "opensessions: missing scripts at $SCRIPTS_DIR"
      exit 0
    fi

    # Best-effort graceful shutdown.
    ${pkgs.curl}/bin/curl -s -o /dev/null -X POST "http://''${HOST}:''${PORT}/shutdown" 2>/dev/null || true

    # Kill stale pid if still present.
    if [ -f "$PID_FILE" ]; then
      kill "$(cat "$PID_FILE")" 2>/dev/null || true
      rm -f "$PID_FILE"
    fi

    # If an opensessions server is still bound on the port, force-kill listener pids.
    if ${pkgs.curl}/bin/curl -s -m 1 "http://''${HOST}:''${PORT}/" 2>/dev/null | grep -q '^opensessions server$'; then
      for pid in $(lsof -tiTCP:"$PORT" -sTCP:LISTEN 2>/dev/null || true); do
        kill "$pid" 2>/dev/null || true
      done
    fi

    # Ensure the fresh server is up and current window has sidebar state.
    if [ -f "$SCRIPTS_DIR/ensure-sidebar.sh" ]; then
      sh "$SCRIPTS_DIR/ensure-sidebar.sh"
    fi

    ready=0
    attempt=0
    while [ "$attempt" -lt 30 ]; do
      if ${pkgs.curl}/bin/curl -s -m 1 "http://''${HOST}:''${PORT}/" 2>/dev/null | grep -q '^opensessions server$'; then
        ready=1
        break
      fi
      sleep 0.1
      attempt=$((attempt + 1))
    done

    if [ "$ready" -eq 1 ]; then
      tmux display-message "opensessions: restarted"
    else
      tmux display-message "opensessions: restart requested, but server is still unavailable"
    fi
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

    opensessions = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable opensessions sidebar integration";
      };
      key = mkOption {
        type = types.str;
        default = "";
        description = "Direct toggle key for sidebar";
      };
      focusKey = mkOption {
        type = types.str;
        default = "";
        description = "Direct focus key for sidebar";
      };
      width = mkOption {
        type = types.int;
        default = 26;
        description = "Sidebar width in columns";
      };
      sidebarPosition = mkOption {
        type = types.enum [
          "left"
          "right"
        ];
        default = "left";
        description = "Position of the sidebar";
      };
      theme = mkOption {
        type = types.str;
        default = "catppuccin-mocha";
        description = "Theme for opensessions sidebar";
      };
      showWindowDetails = mkOption {
        type = types.bool;
        default = true;
        description = "Show window/pane details in sidebar";
      };
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = "Host for opensessions server";
      };
      port = mkOption {
        type = types.int;
        default = 7391;
        description = "Port for opensessions server";
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
        ++ optionals cfg.opensessions.enable [
          pkgs.bun # opensessions runtime
          pkgs.curl # server control
          pkgs.fzf # sessionizer popup
          pkgs.opensessions
        ];
      };

      # Opensessions activation script - syncs from Nix store and runs bun install
      home.activation.opensessions-setup = mkIf cfg.opensessions.enable ''
        checkout_root="${opensessionsCheckout}"
        current_link="${opensessionsCurrent}"
        runtime_root="${opensessionsRuntimeRoot}"

        ${pkgs.coreutils}/bin/mkdir -p "$runtime_root" "$checkout_root"
        ${pkgs.rsync}/bin/rsync -a --no-perms --delete --exclude node_modules --exclude .git \
          "${opensessionsSource}/" \
          "$checkout_root/"

        # Source is copied from the read-only Nix store; make runtime checkout writable.
        ${pkgs.coreutils}/bin/chmod -R u+w "$checkout_root"

        lock_hash_file="$checkout_root/.bun-lock.sha256"
        current_lock_hash=""
        previous_lock_hash=""
        needs_install=0

        if [ -f "$checkout_root/bun.lock" ]; then
          current_lock_hash="$(${pkgs.coreutils}/bin/sha256sum "$checkout_root/bun.lock" 2>/dev/null | ${pkgs.gawk}/bin/awk '{print $1}' || true)"
          if [ -f "$lock_hash_file" ]; then
            previous_lock_hash="$(${pkgs.coreutils}/bin/cat "$lock_hash_file")"
          fi
        fi

        if [ ! -d "$checkout_root/node_modules" ]; then
          needs_install=1
        elif [ -n "$current_lock_hash" ] && [ "$current_lock_hash" != "$previous_lock_hash" ]; then
          needs_install=1
        fi

        if [ "$needs_install" -eq 1 ]; then
          echo "Bootstrapping opensessions dependencies at $checkout_root..." >&2
          if (cd "$checkout_root" && ${pkgs.bun}/bin/bun install --silent --frozen-lockfile 2>/dev/null || ${pkgs.bun}/bin/bun install --silent); then
            if [ -n "$current_lock_hash" ]; then
              ${pkgs.coreutils}/bin/printf '%s\n' "$current_lock_hash" > "$lock_hash_file"
            fi
          else
            echo "Warning: bun install failed; opensessions may be incomplete." >&2
          fi
        fi

        ${pkgs.coreutils}/bin/rm -rf "$current_link"
        ${pkgs.coreutils}/bin/ln -s "$checkout_root" "$current_link"
      '';

      # Seed opensessions config if missing
      home.activation.opensessions-config = mkIf cfg.opensessions.enable ''
        config_dir="$HOME/.config/opensessions"
        config_path="$config_dir/config.json"

        if [ ! -f "$config_path" ]; then
          ${pkgs.coreutils}/bin/mkdir -p "$config_dir"
          ${pkgs.coreutils}/bin/printf '%s\n' ${lib.escapeShellArg opensessionsConfigJson} > "$config_path"
        fi
      '';

      xdg.configFile."tmux/opensessions.sh" = mkIf cfg.opensessions.enable {
        source = opensessionsLoader;
        executable = true;
      };

      xdg.configFile."tmux/restart-opensessions.sh" = mkIf cfg.opensessions.enable {
        source = opensessionsRestart;
        executable = true;
      };

      programs = {
        tmux = {
          aggressiveResize = true;
          baseIndex = 1;
          enable = true;
          terminal = "screen-256color";
          clock24 = true;
          plugins = with pkgs; [
            # bind is
            # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/misc/tmux-plugins/default.nix#L269
            #tmuxPlugins.fzf-tmux-url
            # tmuxPlugins.prefix-highlight
          ];
          customPaneNavigationAndResize = true;
          escapeTime = 0;
          historyLimit = 50000;
          keyMode = "emacs";
          # keyMode = "vi";
          #shortcut = "Space";
          shortcut = "n";
          #prefix = "C-Space";
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
            set -g status-left "#[fg=colour232,bg=colour117] #{?client_prefix,#[fg=white],} #S #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]"
            set -g status-right "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] %Y-%m-%d  %H:%M #[fg=colour117,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour232,bg=colour117] #h "
            setw -g window-status-format "#[fg=colour60,bg=colour60] #I #[fg=colour60,bg=colour60] #W "
            setw -g window-status-current-format "#[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]#[fg=colour146,bg=colour60] #I #[fg=colour146,bg=colour60] #W #[fg=colour60,bg=colour60,nobold,nounderscore,noitalics]"
            set -g mouse on
            set -g extended-keys on
            set -g extended-keys-format csi-u
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

            # Opensessions keybindings
            ${lib.optionalString cfg.opensessions.enable ''
              # Opensessions sidebar controls
              bind S run-shell 'sh ${opensessionsCurrent}/integrations/tmux-plugin/scripts/focus.sh' \; display-message "opensessions: focused sidebar"
              bind s run-shell 'sh ${opensessionsCurrent}/integrations/tmux-plugin/scripts/toggle.sh' \; display-message "opensessions: toggled sidebar"
              bind O run-shell '$XDG_CONFIG_HOME/tmux/restart-opensessions.sh'

              # Load opensessions plugin on startup
              set-environment -g OPENSESSIONS_DIR "${opensessionsCurrent}"
              set-environment -g BUN_PATH "${pkgs.bun}/bin/bun"
              set-environment -g OPENSESSIONS_HOST "${cfg.opensessions.host}"
              set-environment -g OPENSESSIONS_PORT "${toString cfg.opensessions.port}"
              set -g @opensessions-width "${toString cfg.opensessions.width}"
              run-shell '$XDG_CONFIG_HOME/tmux/opensessions.sh'
            ''}

            # keep this at the bottom
            bind-key Tab display-menu -T "#[align=centre]Sessions" "Switch" . 'choose-session -Zw' Last l "switch-client -l" ${tmuxMenuSeperator} \
              "Open Main Workspace" m "display-popup -E \" td ${cfg.mainWorkspaceDir} \"" ${tmuxMenuSeperator} \
              "Kill Current Session" k "run-shell 'tmux switch-client -n \; tmux kill-session -t #{session_name}'"  "Kill Other Sessions" o "display-popup -E \"tkill \"" ${tmuxMenuSeperator} \
              Random r "run-shell 'tat random'" Ollama a "run-shell 'tat ollama'" ${tmuxMenuSeperator} \
              ${lib.optionalString cfg.opensessions.enable ''"Toggle Sidebar" s "run-shell 'sh ${opensessionsCurrent}/integrations/tmux-plugin/scripts/toggle.sh'" ${tmuxMenuSeperator}''} \
              Exit q detach"
          '';
        };
        zsh = {
          sessionVariables = {
            # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tmux#configuration-variables
            # Disable global autostart; we conditionally autostart per-terminal below
            ZSH_TMUX_AUTOSTART = "false";
            ZSH_TMUX_CONFIG = "$XDG_CONFIG_HOME/tmux/tmux.conf";
          }
          // lib.optionalAttrs cfg.opensessions.enable {
            OPENSESSIONS_DIR = opensessionsCurrent;
          };
          shellAliases = {
            # tkill =
            #   "tmux list-sessions -F '#{?session_attached,,#{session_name}}' | sed '/^$/d' | fzf --reverse --header kill-session --preview 'tmux capture-pane -pt {}'  | xargs tmux kill-session -t";

            # tkill =
            #   "for s in $(tmux list-sessions | awk '{print $1}' | rg ':' -r '' | fzf); do tmux kill-session -t $s; done;";
          }
          // lib.optionalAttrs cfg.opensessions.enable {
            osr = "$XDG_CONFIG_HOME/tmux/restart-opensessions.sh";
            osf = "sh ${opensessionsCurrent}/integrations/tmux-plugin/scripts/focus.sh";
            ost = "sh ${opensessionsCurrent}/integrations/tmux-plugin/scripts/toggle.sh";
          };
          initContent = mkAfter ''
            # Auto-start tmux only in Alacritty (not in WezTerm, etc.)
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
