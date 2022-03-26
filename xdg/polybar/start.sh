#!/bin/bash

killall -q polybar

for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar --reload default-bar &
    exit 0
done
