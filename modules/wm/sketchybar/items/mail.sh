#!/usr/bin/env sh
# shellcheck source=$CONFIG_DIR/colors.sh
# shellcheck source=$CONFIG_DIR/icons.sh
. "$CONFIG_DIR/colors.sh" # Loads all defined colors
. "$CONFIG_DIR/icons.sh"  # Loads all defined icons

sketchybar --add item mail left \
  --subscribe mail system_woke \
  --set mail update_freq=60 \
  updates=on \
  script="$PLUGIN_DIR/mail.sh" \
  click_script="$PLUGIN_DIR/mail.sh" \
  icon.padding_left=22 \
  icon.font="$ICON_FONT:Solid:16.0" \
  icon="$MAIL" \
  icon.color="$BLUE" \
  label=!
