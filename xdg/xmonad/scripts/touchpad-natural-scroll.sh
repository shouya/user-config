#!/bin/bash

# Cheatsheet:
#
# - xinput list
# - xinput list-prop "$dev_id"

dev="Synaptics TM3625-010"

xinput set-prop "$dev" 'libinput Tapping Enabled' 1
xinput set-prop "$dev" 'libinput Natural Scrolling Enabled' 1
