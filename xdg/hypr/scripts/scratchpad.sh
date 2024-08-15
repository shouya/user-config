#!/bin/bash -e

cmd="$1"
class="$2"

jq_filter=".class == \"$class\" and .floating == true"

active_window="$(hyprctl -j activewindow | jq -c "select($jq_filter)")"

# if the window is active, hide it
if [[ -n "$active_window" ]]; then
    addr="$(echo "$active_window" | jq -r .address)"
    hyprctl dispatch movetoworkspacesilent "special:scratch,address:$addr"
    exit
fi

window="$(hyprctl -j clients | jq -c ".[] | select($jq_filter)" | head -n1)"

# if the window already exists, show it
if [[ -n "$window" ]]; then
    addr="$(echo "$window" | jq -r .address)"
    hyprctl --batch "dispatch movetoworkspace +0,address:$addr ; dispatch focuswindow address:$addr"
    exit
fi

# otherwise, launch it
hyprctl dispatch exec "[float] $cmd"
