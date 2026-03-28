{ pkgs, lib, ... }:
let
  jq = lib.getExe pkgs.jq;

  # Read payload from stdin, extract transcript path, parse first user message
  # as session label, then send a platform notification.
  notifyScript =
    notify:
    pkgs.writeShellScript "claude-notify" ''
      payload=$(cat)
      transcript=$(echo "$payload" | ${jq} -r '.transcript_path // empty')
      session_label=""
      if [ -n "$transcript" ] && [ -f "$transcript" ]; then
        session_label=$(${jq} -r '
          select(.role == "user") |
          .content |
          if type == "array" then .[0].text else . end |
          split("\n")[0] |
          .[0:60]
        ' "$transcript" 2>/dev/null | head -1)
      fi
      if [ -z "$session_label" ]; then
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
