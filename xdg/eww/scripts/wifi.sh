#!/bin/bash

DEV="${1:-$(ip link | grep wlp | awk '{print $2}' | sed 's/://')}"

print_status() {
  ssid="$(iwgetid -r)"
  freq=""
  addr=""

  if [[ -n "$ssid" ]]; then
    addr="$(ip -o -h addr show dev "$DEV" | grep -oP 'inet6? \K[^ ]+' | paste -sd,)"
    freq="$(iwgetid -f | grep -oP 'Frequency:\K.*')"
    signal="$(iwconfig "$DEV" | grep 'Link Quality' | sed -E -e 's/^\s+//' -e "s/\s+$//")"
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
