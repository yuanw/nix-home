{ pkgs, lib, ... }:
let
  notifyCommand =
    _title: _message:
    if pkgs.stdenv.hostPlatform.isDarwin then
      ''
        ${lib.getExe pkgs.terminal-notifier} -title "Claude Code" -message "Awaiting your input" -sender "com.anthropic.claudecode" -sound default 2>/dev/null || ${lib.getExe pkgs.terminal-notifier} -title "Claude Code" -message "Awaiting your input" -sound default
      ''
    else
      ''notify-send -a "Claude Code" -i "$HOME/.local/share/icons/claude.ico" 'Claude Code' 'Awaiting your input' '';
in
{
  Notification = [
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = notifyCommand "Claude Code" "Awaiting your input";
        }
      ];
    }
  ];
}
