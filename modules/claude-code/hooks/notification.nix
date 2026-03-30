{ pkgs, lib, ... }:
let
  jq = lib.getExe pkgs.jq;

  darwinNotify = pkgs.writeShellScript "claude-notify" ''
    payload=$(cat)
    cwd=$(echo "$payload" | ${jq} -r '.cwd // empty')
    session_label=$([ -n "$cwd" ] && basename "$cwd" || echo "Claude Code")
    msg=$(echo "$payload" | ${jq} -r '.message // "" | split("\n")[0] | .[0:80]')
    notification_type=$(echo "$payload" | ${jq} -r '.notification_type // empty')

    if [ "$notification_type" = "permission_prompt" ]; then
      osascript -e "display notification \"$msg\" with title \"$session_label\" sound name \"Ping\"" &
    else
      osascript -e "display notification \"$msg\" with title \"$session_label\"" &
    fi
  '';

  linuxNotify = pkgs.writeShellScript "claude-notify" ''
    payload=$(cat)
    cwd=$(echo "$payload" | ${jq} -r '.cwd // empty')
    session_label=$([ -n "$cwd" ] && basename "$cwd" || echo "Claude Code")
    msg=$(echo "$payload" | ${jq} -r '.message // "" | split("\n")[0] | .[0:80]')
    notification_type=$(echo "$payload" | ${jq} -r '.notification_type // empty')

    if [ "$notification_type" = "permission_prompt" ]; then
      notify-send -a 'Claude Code' -u critical -i "$HOME/.local/share/icons/claude.ico" "$session_label" "$msg" &
    else
      notify-send -a 'Claude Code' -i "$HOME/.local/share/icons/claude.ico" "$session_label" "$msg" &
    fi
  '';

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
