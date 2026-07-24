{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.herdr;
  tmuxEnabled = config.modules.tmux.enable or false;
  piEnabled = config.modules.pi.enable or false;
  claudeEnabled = config.modules.claude-code.enable or false;

  launchPath = lib.concatStringsSep ":" [
    "/etc/profiles/per-user/${config.my.username}/bin"
    "/run/current-system/sw/bin"
    "${config.my.homeDirectory}/.nix-profile/bin"
    "${config.my.homeDirectory}/.pi/agent/bin"
    "${config.my.homeDirectory}/.local/bin"
    "/opt/homebrew/bin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
  ];

  piThemePalettes = {
    default = {
      base = "#eff1f5";
      surface0 = "#ccd0da";
      surface1 = "#bcc0cc";
      surface2 = "#acb0be";
      text = "#4c4f69";
      subtext1 = "#5c5f77";
      subtext0 = "#6c6f85";
      overlay1 = "#7c7f93";
      mauve = "#8839ef";
      blue = "#1e66f5";
      sapphire = "#209fb5";
      teal = "#179299";
      green = "#40a02b";
      yellow = "#df8e1d";
      peach = "#fe640b";
      red = "#d20f39";
      toolPendingBg = "#e6e9ef";
      toolSuccessBg = "#dcead8";
      toolErrorBg = "#f2d5dc";
    };
    mocha = {
      base = "#1e1e2e";
      surface0 = "#313244";
      surface1 = "#45475a";
      surface2 = "#585b70";
      text = "#cdd6f4";
      subtext1 = "#bac2de";
      subtext0 = "#a6adc8";
      overlay1 = "#7f849c";
      mauve = "#cba6f7";
      blue = "#89b4fa";
      sapphire = "#74c7ec";
      teal = "#94e2d5";
      green = "#a6e3a1";
      yellow = "#f9e2af";
      peach = "#fab387";
      red = "#f38ba8";
      toolPendingBg = "#313244";
      toolSuccessBg = "#1f3a30";
      toolErrorBg = "#3a1f30";
    };
  };

  herdrThemePalettes = {
    default = {
      name = "terminal";
      custom = {
        panel_bg = "reset";
        surface0 = "#ccd0da";
        surface1 = "#bcc0cc";
        surface_dim = "#dce0e8";
        overlay0 = "#8c8fa1";
        overlay1 = "#7c7f93";
        text = "#4c4f69";
        subtext0 = "#6c6f85";
        accent = "#1e66f5";
        blue = "#1e66f5";
        green = "#40a02b";
        yellow = "#df8e1d";
        red = "#d20f39";
        teal = "#179299";
        peach = "#fe640b";
        mauve = "#8839ef";
      };
    };
    catppuccin-auto = {
      name = "terminal";
      custom = {
        panel_bg = "reset";
      };
    };
  };

  piThemeBaseName = "nix-home-herdr";
  piThemeName =
    if cfg.piThemeVariant == "default" then
      piThemeBaseName
    else
      "${piThemeBaseName}-${cfg.piThemeVariant}";
  piThemeVars = piThemePalettes.${cfg.piThemeVariant};
  herdrTheme = herdrThemePalettes.${cfg.themeVariant};

  herdrConfigTemplate =
    if cfg.configFile != null then
      cfg.configFile
    else
      pkgs.writeText "herdr-config.toml" ''
        # Seeded by nix-home. Herdr keeps this file writable after bootstrap.
        [session]
        resume_agents_on_restore = true

        [experimental]
        pane_history = true
      '';

  piThemeFile = pkgs.writeText "${piThemeName}.json" ''
    {
      "$schema": "https://raw.githubusercontent.com/badlogic/pi-mono/main/packages/coding-agent/src/modes/interactive/theme/theme-schema.json",
      "name": "${piThemeName}",
      "vars": {
        "base": "${piThemeVars.base}",
        "surface0": "${piThemeVars.surface0}",
        "surface1": "${piThemeVars.surface1}",
        "surface2": "${piThemeVars.surface2}",
        "text": "${piThemeVars.text}",
        "subtext1": "${piThemeVars.subtext1}",
        "subtext0": "${piThemeVars.subtext0}",
        "overlay1": "${piThemeVars.overlay1}",
        "mauve": "${piThemeVars.mauve}",
        "blue": "${piThemeVars.blue}",
        "sapphire": "${piThemeVars.sapphire}",
        "teal": "${piThemeVars.teal}",
        "green": "${piThemeVars.green}",
        "yellow": "${piThemeVars.yellow}",
        "peach": "${piThemeVars.peach}",
        "red": "${piThemeVars.red}"
      },
      "colors": {
        "accent": "teal",
        "border": "blue",
        "borderAccent": "teal",
        "borderMuted": "surface2",
        "success": "green",
        "error": "red",
        "warning": "yellow",
        "muted": "subtext0",
        "dim": "overlay1",
        "text": "text",
        "thinkingText": "subtext1",
        "selectedBg": "surface0",
        "userMessageBg": "surface0",
        "userMessageText": "text",
        "customMessageBg": "surface0",
        "customMessageText": "text",
        "customMessageLabel": "mauve",
        "toolPendingBg": "${piThemeVars.toolPendingBg}",
        "toolSuccessBg": "${piThemeVars.toolSuccessBg}",
        "toolErrorBg": "${piThemeVars.toolErrorBg}",
        "toolTitle": "sapphire",
        "toolOutput": "text",
        "mdHeading": "mauve",
        "mdLink": "blue",
        "mdLinkUrl": "sapphire",
        "mdCode": "teal",
        "mdCodeBlock": "text",
        "mdCodeBlockBorder": "surface2",
        "mdQuote": "subtext0",
        "mdQuoteBorder": "surface2",
        "mdHr": "surface2",
        "mdListBullet": "teal",
        "toolDiffAdded": "green",
        "toolDiffRemoved": "red",
        "toolDiffContext": "subtext0",
        "syntaxComment": "overlay1",
        "syntaxKeyword": "mauve",
        "syntaxFunction": "blue",
        "syntaxVariable": "peach",
        "syntaxString": "green",
        "syntaxNumber": "peach",
        "syntaxType": "yellow",
        "syntaxOperator": "mauve",
        "syntaxPunctuation": "subtext0",
        "thinkingOff": "surface2",
        "thinkingMinimal": "teal",
        "thinkingLow": "sapphire",
        "thinkingMedium": "blue",
        "thinkingHigh": "mauve",
        "thinkingXhigh": "red",
        "bashMode": "yellow"
      }
    }
  '';

  configBootstrapScript = pkgs.writeText "herdr-config-bootstrap.py" (
    builtins.readFile ./config-bootstrap.py
  );
in
{
  options.modules.herdr = {
    enable = lib.mkEnableOption "herdr";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.herdr;
      description = "Which herdr package to use.";
    };

    command = lib.mkOption {
      type = lib.types.str;
      default = "herdr";
      description = "Herdr binary name on PATH.";
    };

    configFile = lib.mkOption {
      type = lib.types.nullOr (lib.types.either lib.types.str lib.types.path);
      default = null;
      description = "Optional custom config template. When null, a managed template is used.";
    };

    prefix = lib.mkOption {
      type = lib.types.str;
      default = "ctrl+c";
      description = "Herdr key prefix.";
    };

    mainCodingAgent = lib.mkOption {
      type = lib.types.enum [
        "pi"
        "claude"
        "codex"
        "opencode"
      ];
      default = "pi";
      description = "Default coding agent Herdr should prefer.";
    };

    managePiTheme = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install a high-contrast Pi theme tuned for Herdr popups.";
    };

    piThemeVariant = lib.mkOption {
      type = lib.types.enum (lib.attrNames piThemePalettes);
      default = "mocha";
      description = "Palette variant for the managed Pi Herdr theme.";
    };

    themeVariant = lib.mkOption {
      type = lib.types.enum (lib.attrNames herdrThemePalettes);
      default = "catppuccin-auto";
      description = "Herdr UI theme variant applied via config bootstrap.";
    };

    tmuxPopup = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Bind prefix+H to open Herdr in a tmux popup.";
      };

      key = lib.mkOption {
        type = lib.types.str;
        default = "H";
        description = "Tmux key (after prefix) that opens Herdr.";
      };

      width = lib.mkOption {
        type = lib.types.int;
        default = 90;
        description = "Popup width percentage.";
      };

      height = lib.mkOption {
        type = lib.types.int;
        default = 90;
        description = "Popup height percentage.";
      };
    };

    marketplacePlugins = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install common Herdr marketplace plugins on activation.";
      };
    };

    integrations = {
      pi.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install Herdr's Pi integration when modules.pi is enabled.";
      };

      claude.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install Herdr's Claude Code integration when modules.claude-code is enabled.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} =
      hm@{
        pkgs,
        ...
      }:
      {
        home.packages = [ cfg.package ];
        home.sessionVariables.HERDR_MAIN_CODING_AGENT = cfg.mainCodingAgent;

        xdg.configFile."tmux/open-herdr.sh" = {
          executable = true;
          text = ''
            #!${pkgs.bash}/bin/bash
            set -euo pipefail

            if [[ -n "''${PATH:-}" ]]; then
              export PATH='${launchPath}':"$PATH"
            else
              export PATH='${launchPath}'
            fi

            cd "$HOME"

            export PI_COMPUTER_USE_GUI_SESSION_LAUNCH="''${PI_COMPUTER_USE_GUI_SESSION_LAUNCH:-1}"
            export HERDR_MAIN_CODING_AGENT=${lib.escapeShellArg cfg.mainCodingAgent}

            herdr_cmd="''${HERDR_BIN_PATH:-${cfg.command}}"
            export HERDR_BIN_PATH="$herdr_cmd"

            if command -v "$herdr_cmd" >/dev/null 2>&1; then
              state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/herdr"
              version_stamp="$state_dir/launcher-version"
              current_version="$($herdr_cmd --version 2>/dev/null || true)"

              if [[ -n "$current_version" ]]; then
                mkdir -p "$state_dir"
                previous_version="$(cat "$version_stamp" 2>/dev/null || true)"
                if [[ -n "$previous_version" && "$previous_version" != "$current_version" ]]; then
                  echo "open-herdr.sh: Herdr version changed; restart manually if attach fails." >&2
                fi
                printf '%s\n' "$current_version" > "$version_stamp"
              fi

              exec "$herdr_cmd"
            fi

            echo "open-herdr.sh: herdr command not found: $herdr_cmd" >&2
            exec "''${SHELL:-${pkgs.bashInteractive}/bin/bash}" -l
          '';
        };

        programs.tmux.extraConfig = lib.mkIf (cfg.tmuxPopup.enable && tmuxEnabled) (
          lib.mkAfter ''
            # Herdr popup (generated by modules.herdr)
            bind-key ${cfg.tmuxPopup.key} display-popup -E -w ${toString cfg.tmuxPopup.width}% -h ${toString cfg.tmuxPopup.height}% "$HOME/.config/tmux/open-herdr.sh"
          ''
        );

        home.activation.herdr-config-bootstrap = hm.config.lib.dag.entryAfter [ "writeBoundary" ] ''
          herdr_dir="$HOME/.config/herdr"
          target="$herdr_dir/config.toml"
          template="${herdrConfigTemplate}"

          ${pkgs.coreutils}/bin/mkdir -p "$herdr_dir"

          if [ -L "$target" ]; then
            tmp="$(${pkgs.coreutils}/bin/mktemp)"
            ${pkgs.coreutils}/bin/cp -L "$target" "$tmp" 2>/dev/null || ${pkgs.coreutils}/bin/cp "$template" "$tmp"
            ${pkgs.coreutils}/bin/rm -f "$target"
            ${pkgs.coreutils}/bin/mv "$tmp" "$target"
          elif [ ! -e "$target" ]; then
            ${pkgs.coreutils}/bin/cp "$template" "$target"
          fi

          ${pkgs.coreutils}/bin/chmod u+w "$target" 2>/dev/null || true

          ${pkgs.python3}/bin/python3 ${configBootstrapScript} "$target" ${lib.escapeShellArg cfg.prefix} ${lib.escapeShellArg herdrTheme.name} ${lib.escapeShellArg (builtins.toJSON herdrTheme.custom)}
        '';

        home.activation.herdr-marketplace-plugins = hm.config.lib.dag.entryAfter [ "writeBoundary" ] ''
          ${lib.optionalString cfg.marketplacePlugins.enable ''
            export PATH=$PATH:${lib.escapeShellArg launchPath}
            herdr_cmd=${lib.escapeShellArg cfg.command}

            install_plugin() {
              owner="$1"
              repo="$2"
              subdir="''${3:-}"
              mode="''${4:-required}"
              spec="$owner/$repo"
              if [ -n "$subdir" ]; then
                spec="$spec/$subdir"
              fi

              if ! installed_json=$("$herdr_cmd" plugin list --json); then
                echo "herdr: error: failed to list plugins before installing $spec" >&2
                return 1
              fi

              if printf '%s\n' "$installed_json" | ${pkgs.gnugrep}/bin/grep -q "\"owner\":\"$owner\",\"repo\":\"$repo\""; then
                echo "herdr: $spec plugin already installed"
              else
                echo "herdr: installing $spec plugin"
                if ! install_output=$("$herdr_cmd" plugin install "$spec" --yes 2>&1); then
                  printf '%s\n' "$install_output" >&2
                  if [ "$mode" = optional ] && printf '%s\n' "$install_output" | ${pkgs.gnugrep}/bin/grep -Eqi "not found|404|private|permission|could not read Username|authentication"; then
                    echo "herdr: warning: optional $spec plugin unavailable; continuing" >&2
                  else
                    return 1
                  fi
                fi
              fi
            }

            install_plugin smarzban herdr-file-viewer
            install_plugin dutifuldev ghzinga plugins/herdr
            install_plugin ogulcancelik herdr-plugin-github-start
            install_plugin wyattjoh herdr-plugin-gh-pr
            install_plugin kkckkc herdr-plugin-gh-workflow
            install_plugin alon-z herdr-command-palette
            install_plugin 0x5c0f herdr-insight
          ''}
        '';

        home.activation.herdr-agent-integrations =
          hm.config.lib.dag.entryAfter
            [
              "writeBoundary"
              "herdr-marketplace-plugins"
            ]
            ''
              export PATH=$PATH:${lib.escapeShellArg launchPath}
              herdr_cmd=${lib.escapeShellArg cfg.command}

              install_integration() {
                target="$1"
                echo "herdr: installing $target integration"
                "$herdr_cmd" integration install "$target" >/dev/null
              }

              ${lib.optionalString (cfg.integrations.pi.enable && piEnabled) ''
                ${pkgs.coreutils}/bin/mkdir -p "$HOME/.pi/agent/extensions"
                PI_CODING_AGENT_DIR="$HOME/.pi/agent" install_integration pi
              ''}

              ${lib.optionalString (cfg.integrations.claude.enable && claudeEnabled) ''
                ${pkgs.coreutils}/bin/mkdir -p "$HOME/.claude"
                install_integration claude
              ''}
            '';
      };

    modules.pi.themes = lib.mkIf (cfg.managePiTheme && piEnabled) (
      lib.genAttrs [ piThemeName ] (_: {
        src = piThemeFile;
      })
    );
  };
}
