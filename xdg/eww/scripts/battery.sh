#!/bin/bash

acpi -b acpi -b | \
  awk -F'(: |, )' '{gsub("%", ""); print "{\"name\":\""$1"\",\"status\":\""$2"\",\"capacity\":"$3",\"rate\":\""$4"\"}"}' | \
  jq --slurp --compact-output
