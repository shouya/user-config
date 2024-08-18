#!/bin/bash
notify-send "Reloading Hyprland in 2 seconds"
sleep 2
hyprctl reload
notify-send "Hyprland reloaded ($?)"
