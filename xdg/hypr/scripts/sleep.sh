#!/usr/bin/env bash

exec swayidle -w \
            timeout 300 'hyprctl dispatch dpms off' \
            resume 'hyprctl dispatch dpms on' \
            timeout 600 'swaylock -f -c 000000' \
            timeout 900 'systemctl suspend' \
            before-sleep 'swaylock -f -c 000000' &
