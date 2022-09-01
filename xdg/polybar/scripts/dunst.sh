#!/bin/bash

cd "$( dirname "$0" )" >/dev/null 2>&1
. ./common.sh

case "$1" in
  toggle)
    dunstctl set-paused toggle
    ;;

  *)
    dunst="$(action_arg "$0" "toggle" "D")"
    [[ "$(dunstctl is-paused)" == "true" ]] && \
      echo "$(fg "gray" "$dunst")" || \
      echo "$(fg "green" "$dunst")"
    ;;
esac
