#!/bin/bash

DEV="${1:-wlp0s20f3}"

print_status() {
  ssid="$(iwgetid -r)"
  freq=""
  addr=""

  if [[ -n "$ssid" ]]; then
    addr="$(ip -o -h addr show dev "$DEV" | grep -oP 'inet6? \K[^ ]+' | paste -sd,)"
    freq="$(iwgetid -f | grep -oP 'Frequency:\K.*')"
    signal="$(iwconfig wlp0s20f3 | grep 'Link Quality' | sed -E -e 's/^\s+//' -e "s/\s+$//")"
  fi

  cat <<HERE
{"ssid": "$ssid", "addr": "$addr", "freq": "$freq", "signal": "$signal"}
HERE
}

print_status

while true; do
  # monitor the output of ip monitor, until the line containing $DEV
  # appears. When that happens, print_status.
  until ip -o monitor addr link | grep -m 1 "$DEV"; do sleep 1; done

  sleep 1
  print_status
done
