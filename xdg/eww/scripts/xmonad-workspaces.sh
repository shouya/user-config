#!/bin/bash

PROP=_XMONAD_WORKSPACE_LOG

# stdbuf -oL is needed to avoid buffering

if [[ "$WINDOW_MANAGER" != "xmonad" ]]; then
  exit 1
fi


stdbuf -oL xprop -spy -notype -root -f "$PROP" 8t '!$0\n' "$PROP" | \
  stdbuf -oL cut -d! -f2 | \
  stdbuf -oL sed -e 's/^.//' -e 's/.$//'
