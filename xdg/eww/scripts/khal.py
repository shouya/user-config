#!/usr/bin/env python3
import os
import subprocess
import time
import json
import datetime

def zoom_running() -> bool:
    try:
        subprocess.check_output(['pgrep', 'zoom'])
        return True
    except subprocess.CalledProcessError:
        return False

def audio_being_recorded() -> bool:
    try:
        output = subprocess.check_output(['pactl', 'list', 'source-outputs', 'short'])
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

def update_khal() -> bool:
    global item, min_left, name, done

    try:
        result = subprocess.check_output(['khal', 'list', 'now']).decode()
        items = result.strip().split('\n')
        if len(items) < 2:
            done = True
            return False

        done = False
        item = items[1].strip()

        # item looks like "21:15-22:15 foo"
        name = ' '.join(item.split()[1:])

        start_time, end_time = item.split()[0].split('-')

        now = datetime.datetime.now()

        start_time_hour, start_time_minute = start_time.split(':')
        start_time = now.replace(hour=int(start_time_hour), minute=int(start_time_minute))

        end_time_hour, end_time_minute = end_time.split(':')
        end_time = now.replace(hour=int(end_time_hour), minute=int(end_time_minute))

        if now < start_time:
            min_left = (start_time - now).seconds // 60
        else:
            min_left = (end_time - now).seconds // 60

        if now < start_time:
            min_left = (start_time - now).seconds // 60
        else:
            min_left = (end_time - now).seconds // 60
    except (subprocess.CalledProcessError, ValueError):
        return False

def report():
    report = {
        "min_left": min_left,
        "name": name,
        "redacted": redacted,
        "done": done
    }
    new_report = json.dumps(report)
    global last_reported
    if new_report != last_reported:
        print(new_report, flush=True)
        last_reported = new_report


if __name__ == '__main__':
    os.environ['PATH'] = '/home/shou/.asdf/shims:/home/shou/.asdf/bin:/home/shou/.cargo/bin:/home/shou/.local/bin:/home/shou/.shell/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin'

    item = ""
    min_left = 0
    name = ""
    redacted = False
    done = False
    last_reported = ""

    counter = 0

    while True:
        if counter % 60 == 0:
            update_khal()
        if counter % 10 == 0:
            update_redacted()
        report()
        counter += 1
        time.sleep(1)
