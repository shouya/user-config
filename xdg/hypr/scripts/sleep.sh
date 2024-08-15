#!/bin/bash

exec swayidle -w \
            timeout 300 'hyprctl dispatch dpms off' \
            timeout 600 'swaylock -f -c 000000' \
            resume 'hyprctl dispatch dpms on' \
            timeout 900 'systemctl suspend' \
            before-sleep 'swaylock -f -c 000000' &
