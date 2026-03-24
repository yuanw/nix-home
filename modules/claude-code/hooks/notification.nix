{ pkgs, lib, ... }:
let
  notifyCommand =
    title: message:
    if pkgs.stdenv.hostPlatform.isDarwin then
      "${lib.getExe pkgs.terminal-notifier} -title '${title}' -message '${message}'"
    else
      "notify-send -a '${title}' -i \"$HOME/.local/share/icons/claude.ico\" '${title}' '${message}'";
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
