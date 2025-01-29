#!/usr/bin/env bash

focusing_floating="$(hyprctl activewindow | grep "floating: 0")"
direction="$1"

if [[ -z "$focusing_floating" ]]; then
    if [[ "$direction" == "next" ]]; then
        exec hyprctl dispatch cyclenext floating
    else
        exec hyprctl dispatch cyclenext "prev floating"
    fi
else
    if [[ "$direction" == "next" ]]; then
        exec hyprctl dispatch layoutmsg cyclenext
    else
        exec hyprctl dispatch layoutmsg cycleprev
    fi
fi
