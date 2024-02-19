#!/usr/bin/env bash

paths=$*

if [ -z "$paths" ]; then
  echo "No paths were specified, usage: ./zellij-sessionizer path1 path2 etc.."
  exit 0
fi

# Check whether the machine has fd available
if [ -x "$(command -v fd)" ]; then
  selected_path=$(fd . "$paths" --min-depth 1 --max-depth 2 --type d | fzf)
else
  # defer to find if not
  selected_path=$(find "$paths" -mindepth 1 -maxdepth 2 -type d | fzf)
fi

# If nothing was picked, silently exit
if [ -z "$selected_path" ]; then
  exit 0
fi

# If no directory was selected, exit the script
if [ -z "$selected_path" ]; then
  exit 0
fi

# Get the name of the selected directory, replacing "." with "_"
session_name=$(basename "$selected_path" | tr . _)

# We're outside of zellij, so lets create a new session or attach to a new one.
if [ -z "$ZELLIJ" ]; then
  cd "$selected_path" || return

  # -c will make zellij to either create a new session or to attach into an existing one
  zellij attach "$session_name" -c
  exit 0
fi

# We're inside zellij so we'll open a new pane and move into the selected directory
zellij action new-pane

# Hopefully they'll someday support specifying a directory and this won't be as laggy
# thanks to @msirringhaus for getting this from the community some time ago!
zellij action write-chars "cd $selected_path" && zellij action write 10
