#!/bin/bash
#
# Called by inputplug, arguments:
#
# $0 event-type device-id device-type device-name
#

event_type="$1"
device_id="$2"
device_type="$3"
device_name="$4"

if [[ "$device_type" = *Keyboard* && "$event_type" = XIDeviceEnabled ]]; then
  echo "New keyboard detected; configuring..."
  setxkbmap -option ctrl:nocaps,altwin:swap_lalt_lwin
  xset r rate 200 45
fi
