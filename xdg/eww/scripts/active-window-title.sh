#!/bin/bash

PROP=_XMONAD_TITLE_LOG

stdbuf -oL xprop -spy -notype -root -f "$PROP" 8t '!$0\n' "$PROP" | \
  stdbuf -oL cut -d! -f2 | \
  stdbuf -oL sed \
         -e 's/^<field not available>$//' \
         -e 's/^.//' \
         -e 's/.$//'
