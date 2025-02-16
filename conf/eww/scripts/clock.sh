#!/usr/bin/env bash

# a clock that's accurate within few milliseconds

# stolen from https://stackoverflow.com/questions/33204838/bash-command-wait-until-next-full-second
sleep_to_next_second(){
  sleep 0.$(printf '%04d' $((10000 - 10#$(date +%4N))))
}


while true; do
  date +'{"short":"%a %b %e %H:%M:%S", "long": "%c"}'
  sleep_to_next_second
done
