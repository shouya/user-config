#!/usr/bin/env bash

if ! pgrep 'ibus-daemon' 2>/dev/null; then
  ibus-daemon -drx
  ibus engine chewing
  notify-send "Current IBus engine: chewing"
else
  case "$(ibus engine)" in
    "chewing")
      ibus engine xkb:us::eng
      notify-send "Current IBus engine: xkb:us::eng"
      ;;

    "xkb:us::eng")
      ibus engine chewing
      notify-send "Current IBus engine: chewing"
      ;;

    *)
      ibus engine xkb:us::eng
      notify-send "Invalid IBus engine, switching to xkb:us::eng"
      ;;
  esac
fi
