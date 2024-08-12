#!/usr/bin/env python
import os
import subprocess
import socket


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


def get_hyprland_title():
    socket_path = os.path.join(
        os.environ.get("XDG_RUNTIME_DIR", ""),
        "hypr",
        os.environ.get("HYPRLAND_INSTANCE_SIGNATURE", ""),
        ".socket2.sock",
    )

    if not os.path.exists(socket_path):
        print("Hyprland socket not found", flush=True)
        return

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.connect(socket_path)

    for line in sock.makefile("r"):
        line = line.strip()
        if line.startswith("activewindow>>"):
            app, title = line.split(">>")[1].split(",", 1)
            print(f"{app} - {title}", flush=True)


def get_active_window_title():
    desktop_env = os.environ.get("XDG_CURRENT_DESKTOP", "").lower()
    if "hyprland" in desktop_env:
        get_hyprland_title()
    else:
        get_xmonad_title()


if __name__ == "__main__":
    get_active_window_title()
