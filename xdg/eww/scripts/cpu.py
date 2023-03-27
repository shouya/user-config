#!/usr/bin/env python3

import subprocess
import os
import sys
import time
import json
import re
import shutil

# global values
cpus = os.cpu_count()

def check():
    if not shutil.which('cpufreq-info'):
        print('Please install cpufreq-info')
        sys.exit(1)
    if not shutil.which('notify-send'):
        print('Please install notify-send')
        sys.exit(1)
    if not os.path.isfile('/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor'):
        print('CPU freq governor not found')
        sys.exit(1)

def get_freq():
    with open('/sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq') as f:
        freq = f.read().strip()
        freq = float(freq) / 1000 / 1000 # Convert Hz to GHz
        freq = f'{freq:.1f} GHz' # Format the GHz value to one decimal place
    return freq

def run():
    load = os.getloadavg()[0]

    if load > cpus:
        load_level = 'alert'
    elif load > cpus / 3.0:
        load_level = 'warn'
    else:
        load_level = 'ok'

    freq = get_freq()

    # Get CPU temperature
    with open('/sys/class/thermal/thermal_zone0/temp', 'r') as f:
        temp = f.read().strip()
        # Convert millidegrees Celsius to degrees Celsius
        temp = int(int(temp) / 1000)

    output = {'load': load, 'freq': freq, 'temp': temp, 'load_level': load_level}
    print(json.dumps(output), flush=True)

def change_policy():
    curr_governor = ''
    with open('/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor') as f:
        curr_governor = f.read().strip()

    if curr_governor == 'powersave':
        new_governor = 'performance'
    elif curr_governor == 'performance':
        new_governor = 'powersave'
    else:
        new_governor = 'powersave'

    sys_path = "/sys/devices/system/cpu/cpu*/cpufreq/scaling_governor"
    with open(sys_path, 'w') as f:
        f.write(new_governor)


    subprocess.check_call(['notify-send', f'Switching cpu governor policy to {new_governor}'])

    if new_governor == 'performance':
        # automatically switch governor to powersave after 5 minutes
        time.sleep(5 * 60)
        subprocess.check_call(['sudo', 'sh', '-c', f'echo powersave > /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor'])
        subprocess.check_call(['notify-send', 'CPU governor policy restored.'])

if __name__ == '__main__':
    if sys.argv[1] == 'check':
        check()
    elif sys.argv[1] == 'run':
        run()
    elif sys.argv[1] == 'run_forever':
        while True:
            run()
            time.sleep(1)
    elif sys.argv[1] == 'change_policy':
        change_policy()
