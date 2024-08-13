#!/bin/bash -e

start=$(date +%s)
while true; do
  monitor_count="$(hyprctl monitors -j | jq '[.[] | select(.disabled | not)] | length')"
  if [[ "$monitor_count" -eq 1 ]]; then
    break
  fi

  # only try for 1 minute
  if [[ "$(($(date +%s) - start))" -gt 300 ]]; then
    echo "Timed out waiting for monitor to be disabled"
    exit 1
  fi

  sleep 1
done
