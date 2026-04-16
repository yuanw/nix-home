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

  # Opensessions source and version (from patched package)
  opensessionsSrc = "${pkgs.opensessions}/share/opensessions";
  opensessionsVersion = pkgs.opensessions.version;

  opensessionsConfigJson = builtins.toJSON {
    mux = "tmux";
    plugins = [ ];
    theme = {
      palette = {
        base = "#1e1e2e";
        mantle = "#1e1e2e";
        crust = "#1e1e2e";
        surface0 = "#313244";
        surface1 = "#45475a";
        surface2 = "#585b70";
        text = "#cdd6f4";
        subtext0 = "#a6adc8";
        subtext1 = "#bac2de";
        mauve = "#cba6f7";
        lavender = "#b4befe";
        teal = "#94e2d5";
        blue = "#89b4fa";
      };
    };
    sidebarWidth = cfg.opensessions.width;
    inherit (cfg.opensessions) sidebarPosition showWindowDetails;
  };

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

    opensessions = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable opensessions sidebar integration";
      };
      dataDir = mkOption {
        type = types.str;
        default = "${config.my.homeDirectory}/.local/share/opensessions/plugin";
        defaultText = "~/.local/share/opensessions/plugin";
        description = "Directory for opensessions plugin data";
      };
      key = mkOption {
        type = types.str;
        default = "";
        example = "M-s";
        description = "Direct toggle key for sidebar (no prefix required, e.g. 'M-s' for Alt+s, empty to disable)";
      };
      focusKey = mkOption {
        type = types.str;
        default = "";
        example = "M-S";
        description = "Direct focus key for sidebar (no prefix required, empty to disable)";
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
        description = "Theme for opensessions sidebar (unused - theme set in config)";
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
          pkgs.bun
          pkgs.curl
          pkgs.fzf
        ];
      };

      # Declarative config file (shekohex approach)
      xdg.configFile."opensessions/config.json".text = opensessionsConfigJson;

      xdg.configFile."tmux/opensessions.sh" = mkIf cfg.opensessions.enable {
        source = pkgs.writeShellScript "opensessions-init.sh" ''
          set -euo pipefail
          export PATH="${pkgs.coreutils}/bin:${pkgs.bun}/bin:$PATH"

          src="${opensessionsSrc}"
          target="${cfg.opensessions.dataDir}"
          version="${opensessionsVersion}"
          stamp="$target/.version"

          mkdir -p "$(dirname "$target")"

          # Only copy if version changed
          if [[ ! -f "$stamp" || "$(cat "$stamp" 2>/dev/null)" != "$version" ]]; then
            tmp="$(mktemp -d "${cfg.opensessions.dataDir}.tmp.XXXXXX")"
            cp -R "$src/." "$tmp/"
            chmod -R u+w "$tmp"
            if [[ -d "$target" ]]; then
              chmod -R u+w "$target" || true
              rm -rf "$target"
            fi
            mv "$tmp" "$target"
            printf '%s' "$version" > "$stamp"
          fi

          # Run bun install if node_modules missing or lock file changed
          if [[ ! -d "$target/node_modules" ]] || [[ "$target/bun.lock" -nt "$target/node_modules" ]]; then
            echo "Installing opensessions dependencies..." >&2
            (cd "$target" && ${pkgs.bun}/bin/bun install --silent) || true
          fi

          exec "$target/opensessions.tmux"
        '';
        executable = true;
      };

      xdg.configFile."tmux/restart-opensessions.sh" = mkIf cfg.opensessions.enable {
        source = pkgs.writeShellScript "restart-opensessions.sh" ''
          #!/usr/bin/env sh
          set -eu

          target="${cfg.opensessions.dataDir}"
          SCRIPTS_DIR="$target/integrations/tmux-plugin/scripts"
          HOST="${cfg.opensessions.host}"
          PORT="${toString cfg.opensessions.port}"

          if [ ! -d "$SCRIPTS_DIR" ]; then
            tmux display-message "opensessions: missing scripts at $SCRIPTS_DIR"
            exit 0
          fi

          # Best-effort graceful shutdown
          ${pkgs.curl}/bin/curl -s -o /dev/null -X POST "http://''${HOST}:''${PORT}/shutdown" 2>/dev/null || true

          # Kill stale pid if still present
          PID_FILE="/tmp/opensessions.pid"
          if [ -f "$PID_FILE" ]; then
            kill "$(cat "$PID_FILE")" 2>/dev/null || true
            rm -f "$PID_FILE"
          fi

          # If server still bound on port, force-kill listener pids
          if ${pkgs.curl}/bin/curl -s -m 1 "http://''${HOST}:''${PORT}/" 2>/dev/null | grep -q '^opensessions server$'; then
            for pid in $(${pkgs.lsof}/bin/lsof -tiTCP:"$PORT" -sTCP:LISTEN 2>/dev/null || true); do
              kill "$pid" 2>/dev/null || true
            done
          fi

          # Ensure fresh server is up
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
            tmux display-message "opensessions: restart failed"
          fi
        '';
        executable = true;
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

            # Opensessions keybindings
            ${lib.optionalString cfg.opensessions.enable ''
              # Sidebar controls (prefix + key)
              #bind S run-shell 'sh ${cfg.opensessions.dataDir}/integrations/tmux-plugin/scripts/focus.sh' \; display-message "opensessions: focused sidebar"
              #bind s run-shell 'sh ${cfg.opensessions.dataDir}/integrations/tmux-plugin/scripts/toggle.sh' \; display-message "opensessions: toggled sidebar"
              bind O run-shell '$XDG_CONFIG_HOME/tmux/restart-opensessions.sh'

              # Direct keybindings (no prefix required)
              ${lib.optionalString (cfg.opensessions.key != "")
                ''bind-key -n ${cfg.opensessions.key} run-shell 'sh ${cfg.opensessions.dataDir}/integrations/tmux-plugin/scripts/toggle.sh' \; display-message "opensessions: toggled sidebar"''
              }
              ${lib.optionalString (cfg.opensessions.focusKey != "")
                ''bind-key -n ${cfg.opensessions.focusKey} run-shell 'sh ${cfg.opensessions.dataDir}/integrations/tmux-plugin/scripts/focus.sh' \; display-message "opensessions: focused sidebar"''
              }

              # Environment for opensessions
              set-environment -g OPENSESSIONS_DIR "${cfg.opensessions.dataDir}"
              set-environment -g BUN_PATH "${pkgs.bun}/bin/bun"
              set-environment -g OPENSESSIONS_HOST "${cfg.opensessions.host}"
              set-environment -g OPENSESSIONS_PORT "${toString cfg.opensessions.port}"
              set -g @opensessions-width "${toString cfg.opensessions.width}"
              set -g allow-passthrough on

              # Load opensessions on startup
              run-shell '$XDG_CONFIG_HOME/tmux/opensessions.sh'
            ''}

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
              Random r "run-shell 'tat random'" Ollama a "run-shell 'tat ollama'" ${tmuxMenuSeperator} \
              ${lib.optionalString cfg.opensessions.enable ''"Toggle Sidebar" s "run-shell 'sh ${cfg.opensessions.dataDir}/integrations/tmux-plugin/scripts/toggle.sh'" ${tmuxMenuSeperator}''} \
              Exit q detach"
          '';
        };
        zsh = {
          sessionVariables = {
            ZSH_TMUX_AUTOSTART = "false";
            ZSH_TMUX_CONFIG = "$XDG_CONFIG_HOME/tmux/tmux.conf";
          }
          // lib.optionalAttrs cfg.opensessions.enable {
            OPENSESSIONS_DIR = cfg.opensessions.dataDir;
          };
          shellAliases = lib.optionalAttrs cfg.opensessions.enable {
            osr = "$XDG_CONFIG_HOME/tmux/restart-opensessions.sh";
            osf = "sh ${cfg.opensessions.dataDir}/integrations/tmux-plugin/scripts/focus.sh";
            ost = "sh ${cfg.opensessions.dataDir}/integrations/tmux-plugin/scripts/toggle.sh";
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
