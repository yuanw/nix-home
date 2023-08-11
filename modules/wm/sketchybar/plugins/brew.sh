#!/bin/bash
# shellcheck disable=SC1091
. "$CONFIG_DIR/colors.sh" # Loads all defined colors
# shellcheck disable=SC1091
. "$CONFIG_DIR/icons.sh" # Loads all defined icons

COUNT=$(brew outdated | wc -l | tr -d ' ')

COLOR=$RED

case "$COUNT" in
[3-5][0-9])
  COLOR=$ORANGE
  ;;
[1-2][0-9])
  COLOR=$YELLOW
  ;;
[1-9])
  COLOR=$WHITE
  ;;
0)
  COLOR=$GREEN
  COUNT=􀆅
  ;;
esac

sketchybar --set "$NAME" label="$COUNT" icon.color="$COLOR"
