#!/bin/bash

to_color() {
  case "$1" in
    "#"*)
      echo "$1"

      ;;

    red)    echo "#f00" ;;
    yellow) echo "#0ff" ;;
    green)  echo "#0f0" ;;
    action) echo "#DBE194" ;;
  esac
}

escape() {
  echo "${1//\\%/\\%}"
}

escape_cmd() {
  local x
  x="${1//:/\\:}"
  echo "${x//\\%/\\%}"
}

# fg color text
fg() {
  echo "%{F$(to_color "$1")}$(escape "$2")%{F-}"
}

# underline color text
underline() {
  echo "%{u$(to_color "$1")}%{+u}$(escape "$2")%{-u}"
}

# action_no_deco command text
action_no_deco() {
  echo "%{A1:$(escape_cmd "$1"):}$(escape "$2")%{A}"
}

# action command text
action() {
  local a
  a="$(action_no_deco "$1" "$2")"
  underline action "$a"
}

# action command arg text
action_arg() {
  action "$1 \"$2\"" "$3"
}
