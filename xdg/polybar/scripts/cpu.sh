#!/bin/zsh

cd "$( dirname "$0" )" >/dev/null 2>&1

. ./common.sh

check() {
  if ! command -v cpufreq-info; then
    echo "Please install cpufreq-info"
    exit 1
  fi
  if ! command -v notify-send; then
    echo "Please install notify-send"
    exit 1
  fi
  if [[ ! -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]] ; then
    echo "CPU freq governor not found"
    exit 1
  fi

  exit 0
}

run() {
  local freq load governor short_governor load_color

  freq="$(cpufreq-info -c0 -m -f)"
  load="$(cat /proc/loadavg | cut -d' ' -f1)"
  governor="$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)"

  short_governor=""
  case "$governor" in
    powersave)
      short_governor="PS"
      ;;
    performance)
      short_governor="PF"
      ;;
    *)
      short_governor="$governor"
  esac

  load_color=green
  cpus=$(nproc)

  if (( $(( $load > $cpus )) )); then
    load_color=red
  elif (( $(( $load > $cpus / 3.0 )) )); then
    load_color=yellow
  else
    load_color=green
  fi

  echo -n "$(fg "$load_color" "$load")"
  echo " ($freq $(action_arg "$ZSH_ARGZERO" "change_policy" $short_governor))"
}

change_policy() {
  curr_governor="$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)"
  new_governor=""
  case "$curr_governor" in
    powersave)
      new_governor="performance"
      ;;
    performance)
      new_governor="powersave"
      ;;
    *)
      new_governor="powersave"
      ;;
  esac

  echo "$new_governor" | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

  notify-send "switching cpu governor policy to $new_governor"
}

case "$1" in
  check)
    check
    ;;

  run)
    run
    ;;

  change_policy)
    change_policy
    ;;
esac