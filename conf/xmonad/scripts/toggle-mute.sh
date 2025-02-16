#!/usr/bin/env bash

new_status=$(
  amixer set Master playback toggle |
    grep -Eo "\[on\]|\[off\]" | head -n1
)

[[ ! -f /proc/acpi/ibm/volume ]] && exit 0

case "$new_status" in
  "[on]")
    echo "unmute" > /proc/acpi/ibm/volume
    ;;
  "[off]")
    echo "mute" > /proc/acpi/ibm/volume
    ;;
esac
