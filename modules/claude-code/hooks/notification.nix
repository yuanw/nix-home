{ pkgs, lib, ... }:
let
  jq = lib.getExe pkgs.jq;

  # Read payload from stdin, extract transcript path, parse first user message
  # as session label, then send a platform notification.
  notifyScript =
    notify:
    pkgs.writeShellScript "claude-notify" ''
      payload=$(cat)
      cwd=$(echo "$payload" | ${jq} -r '.cwd // empty')
      if [ -n "$cwd" ]; then
        session_label=$(basename "$cwd")
      else
        session_label="Claude Code"
      fi
      msg=$(echo "$payload" | ${jq} -r '.message // .last_assistant_message // "" | split("\n")[0] | .[0:80]')
      ( ${notify} ) &
    '';

  darwinNotify = notifyScript "osascript -e \"display notification \\\"$msg\\\" with title \\\"$session_label\\\"\" &";
  linuxNotify = notifyScript "notify-send -a 'Claude Code' -i \"$HOME/.local/share/icons/claude.ico\" \"$session_label\" \"$msg\"";

  notifyCommand = if pkgs.stdenv.hostPlatform.isDarwin then "${darwinNotify}" else "${linuxNotify}";
in
{
  Notification = [
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = notifyCommand;
        }
      ];
    }
  ];
}
