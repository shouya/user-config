#!/usr/bin/env zsh

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

get_freq() {
  if command -v cpufreq-info &>/dev/null ; then
    cpufreq-info -c0 -m -f
    return
  fi

  if command -v cpupower &>/dev/null ; then
    cpupower frequency-info | \
      grep -E -o "frequency: ([^ ]+ [^ ]+)" | \
      grep -v Unable | \
      sed 's/frequency: //'
    return
  fi
}

run() {
  local freq load governor short_governor load_color

  freq="$(get_freq)"
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

  notify-send "Switching cpu governor policy to $new_governor"

  if [[ "$new_governor" = "performance" ]]; then
    # automatically switch governor to powersave after 5 minutes
    (
      sleep $((5 * 60));
      sh -c "$ZSH_ARGZERO change_policy";
      notify-send "CPU governor policy restored."
    ) &
  fi
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
