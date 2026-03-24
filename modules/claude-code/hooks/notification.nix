{ pkgs, lib, ... }:
{
  "Notification" =
    if pkgs.stdenv.hostPlatform.isDarwin then
      ''
        #!/usr/bin/env bash
        # Use terminal-notifier if available for better icon support, fallback to osascript
        if command -v terminal-notifier &>/dev/null; then
          ${lib.getExe pkgs.terminal-notifier} -title "Claude Code" -message "Awaiting your input" -sender "com.anthropic.claudecode" -sound default 2>/dev/null || \
          ${lib.getExe pkgs.terminal-notifier} -title "Claude Code" -message "Awaiting your input" -sound default
        else
          osascript -e 'display notification "Awaiting your input" with title "Claude Code" sound name "Blow"'
        fi
      ''
    else
      ''
        #!/usr/bin/env bash
        notify-send -a "Claude Code" -i "$HOME/.local/share/icons/claude.ico" 'Claude Code' 'Awaiting your input'
      '';
}
