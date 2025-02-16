#!/usr/bin/env python3
#
# Displays network traffic statistics for all given interface.
#
# Usage example: `python3 net.py`

import time
import sys
import json

INTERVAL = 2


def get_bytes():
    rx_bytes, tx_bytes = 0, 0
    with open("/proc/net/dev") as file:
        for line in file:
            if ":" not in line or "lo:" in line:
                continue
            data = line.split(":")[1].split()
            rx_bytes += int(data[0])
            tx_bytes += int(data[8])
    return rx_bytes, tx_bytes


def human(val):
    units = ["B", "KB", "MB", "GB", "TB", "PB"]
    for unit in units:
        if val < 1024.0:
            if unit == "B":
                return f"{int(val)}"  # show number without the unit
            return f"{val:.0f} {unit}" if val == int(val) else f"{val:.1f} {unit}"
        val /= 1024.0
    return f"{val:.1f} {units[-1]}"


rx, tx = get_bytes()

while True:
    time.sleep(INTERVAL)
    rx_new, tx_new = get_bytes()
    rx_speed = (rx_new - rx) // INTERVAL
    tx_speed = (tx_new - tx) // INTERVAL
    rx_human = human(rx_speed)
    tx_human = human(tx_speed)

    print(
        json.dumps(
            {"rx_bytes": rx_speed, "tx_bytes": tx_speed, "rx": rx_human, "tx": tx_human}
        ),
        flush=True,
    )
    tx, rx = tx_new, rx_new
