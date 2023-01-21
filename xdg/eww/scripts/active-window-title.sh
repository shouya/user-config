#!/bin/bash

update() {
  if ! xdotool getactivewindow getwindowname 2>/dev/null; then
    echo ""
  fi
}

tail -f /tmp/xmonad-title-log 2>&- | \
  while read -r ignore; do
  update
done
