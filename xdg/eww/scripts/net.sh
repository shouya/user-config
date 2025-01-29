#!/usr/bin/env bash

INTERVAL=2
DEVICE="$1"

line="$(grep "$DEVICE" /proc/net/dev | sed s/.*://)"
rx="$(awk '{print $1}' <<<"$line")"
tx="$(awk '{print $9}' <<<"$line")"

while true; do
  sleep "$INTERVAL"
  line="$(grep "$DEVICE" /proc/net/dev | sed s/.*://)"
  rx_new="$(awk '{print $1}' <<<"$line")"
  tx_new="$(awk '{print $9}' <<<"$line")"
  rx_speed=$(((rx_new - rx)/INTERVAL))
  tx_speed=$(((tx_new - tx)/INTERVAL))
  rx_speed_human="$(numfmt --to=iec $rx_speed)"
  tx_speed_human="$(numfmt --to=iec $tx_speed)"

  cat <<HERE
{"rx_bytes":$rx_speed, "tx_bytes":$tx_speed, "rx":"$rx_speed_human", "tx":"$tx_speed_human"}
HERE
  tx="$tx_new"
  rx="$rx_new"
done
