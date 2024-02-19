#!/usr/bin/env bash
# This script is a wrapper for zellij, show a friendly tui interface to deal
# with zellij sessions

# define colors
RED="$(echo -en '\033[0;31m')"
GREEN="$(echo -en '\033[0;32m')"
YELLOW="$(echo -en '\033[0;33m')"
BLUE="$(echo -en '\033[0;34m')"
PURPLE="$(echo -en '\033[0;35m')"
NC="$(echo -en '\033[0m')"

# make sure that dependencies are installed
for package in zellij fzf; do
  command -v "$package" &>/dev/null && continue
  echo -e "${RED}ERROR: $package is not installed!${NC}"
  exit 1
done

# define FZF dracula theme
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9
--color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9
--color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6
--color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4 --ansi --cycle'

# zellij functions #

function attach_session {
  local session_name

  session_name="$(zellij ls |
    fzf --header "Select session to attach" --prompt "Session: " |
    awk '{print $1}')"

  zellij a "$session_name"
  exit 0
}

function create_session {
  read -p "${BLUE}Session name ${RED}(Leave empty to abort)${BLUE}: ${YELLOW}" -r session_name
  echo -en "${NC}"
  [ -z "$session_name" ] && return 1
  zellij -s "$session_name"
  exit 0
}

function delete_session {
  zellij ls | fzf -m --header "Select sessions to delete" |
    awk '{print $1}' | xargs -I {} zellij delete-session {}
  exit 0
}

function list_sessions {
  zellij ls | less -SR
}

function main_script {
  clear
  echo -e "${GREEN}Welcome to zellij runner!${NC}\n\n"

  options=('attach' 'create' 'list' 'delete')
  PS3="${PURPLE}Please enter your choice: ${BLUE}"
  echo -en "${BLUE}"
  select choice in "${options[@]}"; do
    echo -e "${NC}"

    case "$choice" in
    'attach') attach_session ;;
    'create') create_session ;;
    'list') list_sessions ;;
    'delete') delete_session ;;
    esac
  done
}

# Main Script #

action="$1"
if [ -z "$action" ]; then main_script && exit 0; fi

case "$action" in
'attach' | 'a') attach_session ;;
'create' | 'new') create_session ;;
'list' | 'ls') list_sessions ;;
'delete' | 'rm') delete_session ;;
*)
  echo -e "${RED}Invalid action: ${YELLOW}$action${NC}"
  echo -e "${BLUE}Valid actions: ${GREEN}attach|a, create|new, list|ls, delete|rm${NC}"
  exit 1
  ;;
esac
