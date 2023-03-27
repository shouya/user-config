#!/usr/bin/env python

import time
import json

while True:
    time_now = time.localtime()
    short_time = time.strftime("%a %b %d %H:%M:%S", time_now)
    long_time = time.strftime("%a %d %b %Y %I:%M:%S %p %Z", time_now)
    time_json = json.dumps({"short": short_time, "long": long_time})
    print(time_json)


    ns_part = time.time_ns() % 1000000000
    time.sleep(1 - ns_part / 1000000000.0)
