{ pkgs, ... }:
let

  # Thresholds for bash process count
  warnThreshold = 20;
  killThreshold = 100;

  watchdogScript = pkgs.writeShellScript "claude-fork-bomb-watchdog" ''
    count=$(pgrep -u "$USER" bash 2>/dev/null | wc -l | tr -d ' ')

    if [ "$count" -ge ${toString killThreshold} ]; then
      osascript -e "display notification \"Killing $count bash processes now\" with title \"Fork Bomb Detected\" sound name \"Basso\""
      pkill -u "$USER" bash
    elif [ "$count" -ge ${toString warnThreshold} ]; then
      osascript -e "display notification \"$count bash processes running — possible fork bomb\" with title \"Claude Watchdog\" sound name \"Ping\""
    fi
  '';
in
{
  inherit watchdogScript warnThreshold killThreshold;
}
