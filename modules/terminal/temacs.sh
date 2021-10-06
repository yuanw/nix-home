#!/bin/sh
#
# Attach or create tmux session named the same as current directory.
# https://github.com/thoughtbot/dotfiles/blob/master/bin/tat

session_name=emacs

not_in_tmux() {
  [ -z "$TMUX" ]
}

session_exists() {
  tmux has-session -t "=$session_name"
}

create_detached_session() {
  (TMUX=''
  tmux new-session -Ad -s "$session_name"
  tmux send-keys -t "$session_name" "emacs -nw" "C-m"
  )
}

create_if_needed_and_attach() {
  if not_in_tmux; then
    tmux new-session -As "$session_name"
  else
    if ! session_exists; then
      create_detached_session
    fi
    tmux switch-client -t "$session_name"
  fi
}

create_if_needed_and_attach
