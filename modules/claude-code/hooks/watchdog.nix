{ pkgs, ... }:
let

  # Thresholds for bash process count
  warnThreshold = 20;
  killThreshold = 100;

  watchdogScript = pkgs.writeShellScript "claude-fork-bomb-watchdog" ''
    count=$(pgrep -u "$USER" bash 2>/dev/null | wc -l | tr -d ' ')

    if [ "$count" -ge ${toString killThreshold} ]; then
      # Collect the full descendant tree of each claude process before killing,
      # so new children spawned mid-walk are still caught in the same pass.
      descendants() {
        for child in $(pgrep -P "$1" 2>/dev/null); do
          echo "$child"
          descendants "$child"
        done
      }
      all_pids=""
      for claude_pid in $(pgrep -u "$USER" -f "claude" 2>/dev/null); do
        all_pids="$all_pids $(descendants "$claude_pid")"
      done
      # shellcheck disable=SC2086
      echo "$all_pids" | xargs kill 2>/dev/null
      osascript -e "display notification \"Killed $count bash processes (claude descendants)\" with title \"Fork Bomb Stopped\" sound name \"Basso\""
    elif [ "$count" -ge ${toString warnThreshold} ]; then
      osascript -e "display notification \"$count bash processes running — possible fork bomb\" with title \"Claude Watchdog\" sound name \"Ping\""
    fi
  '';
in
{
  inherit watchdogScript warnThreshold killThreshold;
}
