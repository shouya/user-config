#!/bin/bash

get_touchpad_id() {
  xinput list | grep TouchPad | grep -E -o "id=[0-9]+" | sed 's/id=//'
}

get_prop_id() {
  device="$1"
  xinput list-props "$device" |
    grep 'Natural' |
    grep -v 'Default' |
    grep -o -E '\([0-9]+\)' |
    tr -d '()'
}

set_natural_mode() {
  device="$1"
  prop="$2"
  value="$3"

  xinput set-prop "$device" "$prop" "$value"
}


device="$(get_touchpad_id)"
if [[ -z "$device" ]]; then
  echo "Device not found"
  exit 1
fi

prop="$(get_prop_id "$device")"
if [[ -z "$prop" ]]; then
  echo "Property not found for device $device"
  exit 1
fi

set_natural_mode "$device" "$prop" 1
