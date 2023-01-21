#!/bin/bash

# a clock that's accurate within few milliseconds

# stolen from https://stackoverflow.com/questions/33204838/bash-command-wait-until-next-full-second
sleep_to_next_second(){
  sleep 0.$(printf '%04d' $((10000 - 10#$(date +%4N))))
}


while true; do
  date +'{"year":"%Y", "month":"%m", "date": "%d", "hour":"%H","min":"%M","sec":"%S", "nanos": "%N"}'
  sleep_to_next_second
done
