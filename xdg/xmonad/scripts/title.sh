#!/bin/bash

if pgrep zoom &>/dev/null; then
	echo "****"
	exit
fi

export PATH=/home/shou/.asdf/shims:/home/shou/.asdf/bin:/home/shou/.cargo/bin:/home/shou/.local/bin:/home/shou/.shell/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin

khal list now | head -n2 | tail -n1

# if no lines are printed before this, show a message
echo 'Done for today!'


