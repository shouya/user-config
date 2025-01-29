#!/usr/bin/env bash -e

echo balanced | sudo tee /sys/firmware/acpi/platform_profile
notify-send "cpu frequency scheduling reset"
