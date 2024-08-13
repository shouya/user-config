#!/bin/bash

BG="$(find ~/Pictures/wallpapers -type f | shuf | head -n1)"
if [[ -z "$BG" ]]; then
    echo "No wallpapers found"
    exit 1
fi

exec swaybg -i "$BG"
