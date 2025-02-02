#!/usr/bin/env python3
import os
import subprocess
import time
import json
import math
import signal
import sys
import re
from datetime import datetime, date, timedelta


def zoom_running() -> bool:
    try:
        subprocess.check_output(["pgrep", "zoom"])
        return True
    except subprocess.CalledProcessError:
        return False


def audio_being_recorded() -> bool:
    try:
        output = subprocess.check_output(["pactl", "list", "source-outputs", "short"])
        return len(output.decode().strip()) > 0
    except subprocess.CalledProcessError:
        return False


def update_redacted() -> bool:
    global redacted
    if audio_being_recorded():
        redacted = True
        return True
    else:
        redacted = False
        return False


def parse_khal_events(s: str) -> [dict]:
    """
    The output looks like the follows:

    2024-11-24 12:00::2024-11-24 12:45::C: calculus
    2024-11-24 12:45::2024-11-24 13:30::SW: coding
    """

    lines = s.strip().split("\n")
    events = []
    for line in lines:
        line = line.strip()
        if len(line) == 0:
            continue

        start, end, title = line.split("::", 2)
        start_time = parse_time(start)
        end_time = parse_time(end)

        events.append(
            {
                "start_time": start_time,
                "end_time": end_time,
                "name": title,
            }
        )
    return events


def parse_time(s: str) -> time:
    normalized_s = s.replace("24:00", "00:00")
    return datetime.strptime(normalized_s, "%Y-%m-%d %H:%M")


def next_whole_minute(now) -> datetime:
    return now + timedelta(
        minutes=1, seconds=-now.second, microseconds=-now.microsecond
    )


def update_khal() -> bool:
    global min_left, name, done, upcoming

    now = datetime.now()

    try:
        # example: 2024-11-24 12:00::2024-11-24 12:45::C: calculus
        fmt = "{start-long}::{end-long}::{title}"
        result = subprocess.check_output(
            ["khal", "list", "now", "-df", "", "-f", fmt]
        ).decode()
        events = parse_khal_events(result)

        if len(events) < 1:
            done = True
            return

        done = False
        # we show the most recent event
        item = events[0]
        name = item["name"]

        if now < item["start_time"]:
            upcoming = True
            min_left = math.ceil((item["start_time"] - now).seconds / 60)
        else:
            upcoming = False
            min_left = math.ceil((item["end_time"] - now).seconds / 60)
        return

    except (subprocess.CalledProcessError, ValueError) as e:
        print(e, file=sys.stderr)
        return


def report():
    report = {
        "min_left": min_left,
        "name": name,
        "redacted": redacted,
        "done": done,
        "upcoming": upcoming,
    }
    new_report = json.dumps(report)
    global last_reported
    if new_report != last_reported:
        print(new_report, flush=True)
        last_reported = new_report


def handle_hup(signum, frame):
    update_khal()
    update_redacted()
    report()


def main():
    next_update_check = datetime.now()
    next_redact_check = datetime.now()

    signal.signal(signal.SIGHUP, handle_hup)

    while True:
        now = datetime.now()
        if now > next_update_check:
            update_khal()
            next_update_check = next_whole_minute(now)

        if now > next_redact_check:
            update_redacted()
            next_redact_check = now + timedelta(seconds=30)

        report()

        # fmt: off
        sleep_time = min(
            max((next_update_check - now).seconds, 0),
            max((next_redact_check - now).seconds, 0),
            30
        )

        time.sleep(sleep_time)


if __name__ == "__main__":
    min_left = 0
    name = ""
    current_event = None
    redacted = False
    done = False
    last_reported = ""
    upcoming = False
    counter = 0

    main()
