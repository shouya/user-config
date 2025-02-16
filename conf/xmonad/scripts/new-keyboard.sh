#!/usr/bin/env bash
#
# Called by inputplug, arguments:
#
# $0 event-type device-id device-type device-name
#


echo "$@"

event_type="$1"
device_id="$2"
device_type="$3"
device_name="$4"

setup_keyboard() {
  echo "New keyboard detected; configuring..."
  setxkbmap -option ctrl:nocaps,altwin:swap_lalt_lwin
  xset r rate 230 40
}

if [[ "$#" = 0 ]]; then
  setup_keyboard
fi

if [[ "$device_type" = *Keyboard* && "$event_type" = XIDeviceEnabled ]]; then
  setup_keyboard
fi
