#!/usr/bin/env python

import os
import subprocess


def get_xmonad_title():
    prop = "_XMONAD_TITLE_LOG"
    process = subprocess.Popen(
        ["xprop", "-spy", "-notype", "-root", "-f", prop, "8t", "!$0\\n", prop],
        stdout=subprocess.PIPE,
        text=True,
        bufsize=0,
    )
    while True:
        output = process.stdout.readline().strip()
        if not output:
            continue
        title = output.split("!")[1].strip()
        title = title.strip('"')
        if title == "<field not available>":
            print("", flush=True)
            continue

        if title:
            print(title, flush=True)


if __name__ == "__main__":
    get_xmonad_title()
