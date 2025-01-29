#!/usr/bin/env bash

export PATH=/home/shou/.asdf/shims:/home/shou/.asdf/bin:/home/shou/.cargo/bin:/home/shou/.local/bin:/home/shou/.shell/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin

item=""
min_left=0
name=""
redacted=false
done=false
last_reported=""

counter=0

report() {
  read -r -d '' new_report <<EOF
{"min_left": $min_left, "name": "$name", "redacted": $redacted, "done": $done}
EOF

  if [[ "$new_report" = "$last_reported" ]]; then
    return
  else
    echo "$new_report"
    last_reported="$new_report"
  fi
}

zoom_running() {
  pgrep zoom &>/dev/null
}

audio_being_recorded() {
  [[ "$(pactl list source-outputs short | wc -l)" -gt 0 ]]
}

update_redacted() {
  # if zoom_running; then

  if audio_being_recorded; then
    redacted=true
	  return 1
  else
    redacted=false
  fi
}

update_khal() {
  item="$(khal list now | head -n2 | tail -n1)"

  if [[ -z "$item" ]]; then
    # if no lines are printed before this, show a message
    done=true
    return 1
  else
    done=false
  fi

  name="$(cut -d' ' -f2- <<<"$item")"
  start="$(date --date "$(awk -F'-| ' '{print $1}'<<<"$item")" +%s)"
  end="$(date --date "$(awk -F'-| ' '{print $2}'<<<"$item")" +%s)"
  now="$(date +%s)"

  if [[ "$now" -lt "$start" ]]; then
    min_left="-$(((start - now) / 60))"
  else
    min_left="$(((end - now) / 60))"
  fi

  return 0
}

while true; do
  update_redacted

  if [[ "$((counter%60))" -eq 0 ]]; then
    update_khal
  fi

  report
  counter=$((counter+1))
  sleep 1
done
