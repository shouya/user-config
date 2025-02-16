#!/usr/bin/env bash

# Cheatsheet:
#
# - xinput list
# - xinput list-prop "$dev_id"

# dev="Synaptics TM3625-010"
dev="SYNA8013:00 06CB:CE69 Touchpad"

xinput set-prop "$dev" 'libinput Tapping Enabled' 1
xinput set-prop "$dev" 'libinput Natural Scrolling Enabled' 1

touchscreen='Wacom Pen and multitouch sensor Finger touch'
xinput map-to-output "$touchscreen" eDP-1
