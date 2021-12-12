#!/bin/bash

get_status() {
  swaymsg -t get_inputs | jq -r '.[] | select(.identifier == "1:1:AT_Translated_Set_2_keyboard") | .libinput.send_events'
}

case "$1" in
  toggle)
    if [[ "$(get_status)" = "disabled" ]]; then
      action=enable
    else
      action=disable
    fi

    ~/projects/scripts/linux/internal-keyboard.sh "$action"
    ;;

  *|poll)
    # needs a delay to get accurate result after toggling
    sleep 0.1
    if [[ "$(get_status)" = "disabled" ]]; then
      echo "()"
    else
      echo "()"
    fi
    ;;
esac
