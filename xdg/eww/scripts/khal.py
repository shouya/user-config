#!/usr/bin/env python3
import os
import subprocess
import time
import json
import math
import signal
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

    Today, 2023-07-15
    16:00-17:45 P: fix eww calendar latency
    17:45-18:00 H: dishwasher
    17:45-18:00 P: system upgrade
    18:00-20:00 P: dotcam
    """

    lines = s.strip().split("\n")
    events = []
    today = date.today()
    for line in lines:
        line = line.strip()
        if len(line) == 0:
            continue
        if line.startswith("Today,"):
            continue
        if line.startswith("Tomorrow,"):
            break
        time_span, title = line.split(" ", 1)
        start_time, end_time = time_span.split("-", 1)

        start_time = parse_time(start_time)
        end_time = parse_time(end_time)

        start_time = datetime.combine(today, start_time)
        end_time = datetime.combine(today, end_time)

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
    return datetime.strptime(normalized_s, "%H:%M").time()


def next_whole_minute(now) -> datetime:
    return now + timedelta(
        minutes=1, seconds=-now.second, microseconds=-now.microsecond
    )


def update_khal() -> bool:
    global min_left, name, done, upcoming

    now = datetime.now()

    try:
        result = subprocess.check_output(["khal", "list", "now"]).decode()
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

    except (subprocess.CalledProcessError, ValueError):
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
    # for khal cli
    os.environ["PATH"] = ":".join(
        [
            "/home/shou/.asdf/shims",
            "/home/shou/.asdf/bin",
            "/home/shou/.cargo/bin",
            "/home/shou/.local/bin",
            "/home/shou/.shell/bin",
            "/usr/local/bin",
            "/usr/local/sbin",
            "/usr/sbin",
            "/sbin",
            "/usr/local/bin",
            "/usr/bin",
            "/bin",
        ]
    )

    min_left = 0
    name = ""
    current_event = None
    redacted = False
    done = False
    last_reported = ""
    upcoming = False
    counter = 0

    main()
