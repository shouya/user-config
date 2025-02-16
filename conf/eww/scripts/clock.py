#!/usr/bin/env python
# Python script for printing time every second.

import time
import json

SHORT_TIME_FORMAT = "%a %b %d %H:%M:%S"
LONG_TIME_FORMAT = "%a %d %b %Y %I:%M:%S %p %Z"

while True:
    time_now = time.localtime()
    short_time = time.strftime(SHORT_TIME_FORMAT, time_now)
    long_time = time.strftime(LONG_TIME_FORMAT, time_now)
    time_json = json.dumps({"short": short_time, "long": long_time})
    print(time_json, flush=True)

    ns_part = (time.time_ns() % 1000000000) / 1000000000.0
    sleep_time = 1 - ns_part
    time.sleep(sleep_time)
