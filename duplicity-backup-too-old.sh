#!/usr/bin/env bash
DAYS_ALLOWED=${1:-4}

LASTDATE=$(cat /home/pe/.duplicity/default.last_run)
LASTDATE_FMT=$(date --date="$LASTDATE" +%Y-%m-%d)
NOW=$(date +%Y-%m-%d)
DAY_DIFF=$(datediff "$LASTDATE_FMT" "$NOW" -f "%d")
if (( $DAY_DIFF > $DAYS_ALLOWED )); then
  echo "true"
else
  echo "false"
fi
