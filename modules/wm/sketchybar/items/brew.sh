#!/bin/bash

# Trigger the brew_udpate event when brew update or upgrade is run from cmdline
# e.g. via function in .zshrc
# shellcheck disable=SC1091
. "$CONFIG_DIR/colors.sh" # Loads all defined colors
. "$CONFIG_DIR/icons.sh"  # Loads all defined icons

brew=(
  icon=􀐛
  label=?
  padding_right=10
  script="$PLUGIN_DIR/brew.sh"
)

sketchybar --add event brew_update \
  --add item brew right \
  --set brew "${brew[@]}" \
  --subscribe brew brew_update
