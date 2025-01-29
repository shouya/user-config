#!/usr/bin/env bash
#
# This script configures how to handle idle actions and sleep related stuff.
#
#
# IDLE ACTIONS
# ========================================================================
# 1. Notify systemd for idle action
# notify systemd-logind when idle for 30 seconds so logind.conf(5) can
# track "IdleAction"/"IdleActionSec"
#
# 2. After 5 minutes of idle, lock the screen.
#
# 3. After 30 minutes of idle, logind will suspend
#
# 4. Whenever a suspend is requested, lock the screen first ("before-sleep")
#
# 5. Whenever a lock is requested, call the lock command.
#
#
# LID CLOSE ACTIONS
# ========================================================================
#
# 1. The system will try to hybrid sleep ("HandleLidSwitch=hybrid-sleep").
#
# 2. Before sleep, the screen will be locked ("before-sleep").
#
# I want to make it hybrid sleep instead of simple suspending because I expect
# to carry the laptop around (for a medium amount of time) so it's best that it
# doesn't lose state after power runs out.
#

LOCK_CMD="~/.config/sway/scripts/lock.sh"

swayidle \
  idlehint 30 \
  timeout 300 "$LOCK_CMD" \
  before-sleep "$LOCK_CMD" \
  lock "$LOCK_CMD"

# My /etc/systemd/logind.conf is here:
#
# [Login]
# HandlePowerKey=hibernate
# HandleLidSwitch=hybrid-sleep
# HoldoffTimeoutSec=30s
# IdleAction=suspend
# IdleActionSec=30min
#
