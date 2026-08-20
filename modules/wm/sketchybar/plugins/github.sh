#!/bin/bash
# shellcheck disable=SC1091
. "$CONFIG_DIR/colors.sh" # Loads all defined colors
. "$CONFIG_DIR/icons.sh"  # Loads all defined icons

update() {
  NOTIFICATIONS="$(python3 "$CONFIG_DIR/plugins/github-notifications-filter.py")"
  COUNT="$(echo "$NOTIFICATIONS" | jq 'length')"
  args=()
  if [ "$NOTIFICATIONS" = "[]" ]; then
    args+=(--set "$NAME" icon="$BELL" label="0")
  else
    args+=(--set "$NAME" icon="$BELL_DOT" label="$COUNT")
  fi

  PREV_COUNT=$(sketchybar --query github.bell | jq -r .label.value)
  # For sound to play around with:
  # afplay /System/Library/Sounds/Morse.aiff

  args+=(--remove '/github.notification\.*/')

  COUNTER=0
  COLOR=$BLUE
  args+=(--set github.bell icon.color="$COLOR")

  strip() {
    echo "$1" | sed -e "s/^'//" -e "s/'$//" -e 's/^null$//' -e 's/^$//'
  }

  resolve_url() {
    local html_url="$1"
    local api_url="$2"
    html_url="$(strip "$html_url")"
    api_url="$(strip "$api_url")"
    if [ -n "$html_url" ]; then
      echo "$html_url"
      return
    fi
    if [ -n "$api_url" ]; then
      gh api "$api_url" --jq .html_url 2>/dev/null
    fi
  }

  while read -r repo api_url html_url type title; do
    COUNTER=$((COUNTER + 1))
    IMPORTANT="$(echo "$title" | grep -iE "(deprecat|break|broke)")"
    COLOR=$BLUE
    PADDING=0

    if [ "${repo}" = "" ] && [ "${title}" = "" ]; then
      repo="Note"
      title="No new notifications"
    fi
    case "${type}" in
    "'Issue'")
      COLOR=$GREEN
      ICON=$GIT_ISSUE
      URL="$(resolve_url "$html_url" "$api_url")"
      ;;
    "'Discussion'")
      COLOR=$WHITE
      ICON=$GIT_DISCUSSION
      URL="https://www.github.com/notifications"
      ;;
    "'PullRequest'")
      COLOR=$MAGENTA
      ICON=$GIT_PULL_REQUEST
      URL="$(resolve_url "$html_url" "$api_url")"
      ;;
    "'Commit'")
      COLOR=$WHITE
      ICON=$GIT_COMMIT
      URL="$(resolve_url "$html_url" "$api_url")"
      ;;
    esac

    if [ "$IMPORTANT" != "" ]; then
      COLOR=$RED
      ICON=􀁞
      args+=(--set github.bell icon.color="$COLOR")
    fi

    notification=(
      label="$(echo "$title" | sed -e "s/^'//" -e "s/'$//")"
      icon="$ICON $(echo "$repo" | sed -e "s/^'//" -e "s/'$//"):"
      icon.padding_left="$PADDING"
      label.padding_right="$PADDING"
      icon.color="$COLOR"
      position=popup.github.bell
      icon.background.color="$COLOR"
      drawing=on
      click_script="open $URL; sketchybar --set github.bell popup.drawing=off"
    )

    args+=(--clone github.notification."$COUNTER" github.template
      --set github.notification."$COUNTER" "${notification[@]}")
  done <<<"$(echo "$NOTIFICATIONS" | jq -r '.[] | [.repository.name, .subject.url, (.subject.html_url // ""), .subject.type, .subject.title] | @sh')"

  sketchybar -m "${args[@]}" >/dev/null

  if [ "$COUNT" -gt "$PREV_COUNT" ] 2>/dev/null || [ "$SENDER" = "forced" ]; then
    sketchybar --animate tanh 15 --set github.bell label.y_offset=5 label.y_offset=0
  fi
}

popup() {
  sketchybar --set "$NAME" popup.drawing="$1"
}

case "$SENDER" in
"routine" | "forced")
  update
  ;;
"mouse.entered")
  popup on
  ;;
"mouse.exited" | "mouse.exited.global")
  popup off
  ;;
"mouse.clicked")
  popup toggle
  ;;
esac
