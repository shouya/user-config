#!/usr/bin/env python3

import os
import json
import socket
import struct
import subprocess


def get_hyprland_instance_signature():
    return os.environ.get("HYPRLAND_INSTANCE_SIGNATURE")


def get_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR")
    instance_signature = get_hyprland_instance_signature()
    return f"{runtime_dir}/hypr/{instance_signature}/.socket2.sock"


def send_hyprctl_command(command):
    result = subprocess.run(["hyprctl", "-j", command], capture_output=True, text=True)
    return json.loads(result.stdout)


def get_workspace_info():
    workspaces = send_hyprctl_command("workspaces")
    active_workspace = send_hyprctl_command("activeworkspace")
    active_id = active_workspace["id"]

    workspace_info = []
    existing_ids = set()

    for i in range(1, 10):
        workspace = next((w for w in workspaces if w["id"] == i), None)
        entry = {
            "index": i,
            "name": f"{i}",
            # the workspace is visible
            "visible": False,
            # this workspace has focus
            "current": False,
            # the workspace is empty
            "hidden": False,
            # the workspace has urgent windows
            "urgent": False,
            "visibleNoW": True,
            "monitorID": 0,
        }

        if workspace is not None:
            entry.update(
                {
                    "name": workspace["name"],
                    "current": i == active_id,
                    "visible": workspace["windows"] != 0,
                    "hidden": workspace["windows"] != 0,
                    "monitorID": workspace["monitorID"],
                }
            )

        workspace_info.append(entry)

    return workspace_info


def print_workspace_info():
    print(json.dumps(get_workspace_info()), flush=True)


def monitor_hyprland_events():
    socket_path = get_socket_path()
    print_workspace_info()  # Initial workspace information

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.connect(socket_path)

    for line in sock.makefile("r"):
        print_workspace_info()


if __name__ == "__main__":
    if "HYPRLAND_INSTANCE_SIGNATURE" not in os.environ:
        print("HYPRLAND_INSTANCE_SIGNATURE not found", flush=True)
        exit(1)
    monitor_hyprland_events()
