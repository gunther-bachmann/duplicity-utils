#!/usr/bin/env bash
set -euo pipefail

PROFILE="default"               # profile name, no spaces allowed, just regular characters

VERSION="0.2.1"
VERBOSE=false                   # printout information while running
DRYRUN=true                     # check parameter, do not execute duplicity
FAKERUN=false                   # run duplicity but do not make a backup
ENCRYPTION_KEY=""               # use this key for encryption
EXCLUDE_FILELIST=""             # use this list for exclusion (passed to duplicity)
SOURCE_FOLDER=$(realpath ~)     # folder to make actual backup of
BACKUP_FOLDER=""                # folder into which to store the backup made
TEMP_FOLDER=""                  # temporary folder during backup execution
# NOTIFICATIONS=false
NOCPULIMIT=false                # whether to limit the cpu load during backup
FORCE=false                     # force to continue in the face of (configuration) errors
KEEP_N=3                        # used by disabled command cleanup

CPULIMIT=""                     # executable (found) for cpu limit
IONICE=""                       # executable (found) for ionice
NICE=""                         # executable (found) for nice
DUPLICITY=""                    # executable (found) for duplicity
IGNORE_WITHIN_HOURS=1           # ignore calls that are x hours old
BATID="BAT0"                    # battery to check TODO put into configuration

DISCHARGING="Discharging"
MIN_DISCHARGING_LEVEL=50        # minimal battery level for backup to execute TODO put into configuration

BACKUP_MODE="incremental"       # or "full", which will always do a full backup
MONTHS_TO_FULL=1                # in case of incrementals, month difference that forces full backup


export HOST=$(uname -m)
export HOSTNAME=${HOST}

check_battery_level() {
  BATLEVEL=$(cat /sys/class/power_supply/${BATID}/capacity)
  BATSTATE=$(cat /sys/class/power_supply/${BATID}/status)

  if [ "$BATSTATE" = "$DISCHARGING" -a $BATLEVEL -le $MIN_DISCHARGING_LEVEL ]; then
    echo "Battery level too low ($BATLEVEL%). Please plugin."
    [ "$DRYRUN" != "true" ] && exit 1
  else
    [ "$VERBOSE" == "true" ] && echo "Battery level ok ($BATLEVEL%), or plugged in."
  fi
}

find_configuration_folder() {
  # TODO do some file discovery
  printf ~/.duplicity
}

find_configuration_file() {
  conf_folder=$(find_configuration_folder)
  printf ${conf_folder}/config
}

strip_enclosing_quotes() {
  read data
  echo $data | sed "s|'\(.*\)'|\1|g" | sed "s|\"\(.*\)\"|\1|g"
}

nop() {
  NOP=true
}

proc_configuration_line() {
  CONF_LINE=${1:-}
  [ "$VERBOSE" == "true" ] && printf "proc_configuration_line\n"
  if [[ $CONF_LINE == \#* ]]; then
    [ "$VERBOSE" == "true" ] && echo "    skipping configuration line \"$CONF_LINE\""
    nop
  elif [ "$CONF_LINE" == "" ]; then
    [ "$VERBOSE" == "true" ] && echo "    skipping empty configuration line"
    nop
  else
    [ "$VERBOSE" == "true" ] && echo "      using configuration line \"$CONF_LINE\""
    KEY=$(echo "$CONF_LINE" | sed "s|\([a-zA-Z_-]*\): \?\(.*\)|\1|g")
    VALUE=$(echo "$CONF_LINE" | sed "s|\([a-zA-Z_-]*\): \?\(.*\)|\2|g" | strip_enclosing_quotes)
    [ "$VERBOSE" == "true" ] && echo "    using configuration key: \"$KEY\" value: \"$VALUE\""
    proc_option $KEY $VALUE
  fi
}

usage() {
  printf "\n\nUSAGE:\n$0 command [flags|option]*\nduplicity backup controller\n  Control the actual execution of backups.\n\n"
  usage_options
}

usage_options() {
  printf "  commands:\n"
  printf "    print-config                      print current configuration\n"
  printf "    backup                            execute configured backup\n"
  printf "    key-status                        exit code 0 = key is unlocked, else exit code !=0\n"
  printf "    collection-status                 backup collection status\n"
#  printf "    cleanup                           cleanup old backup collections\n"
  printf "\n"
  printf "  flags:\n"
  printf "    [--fake-run]                      run duplicity (and all backup calculations) without actually making a backup.\n"
  printf "                                      this corresponds to the duplicity option of dry run.\n"
  printf "    [--folder]                        use this folder as source instead of the default (home dir, '~')\n"
  printf "    [--force]                         continue in the face of errors (in options)\n"
  printf "    [--keep n]                        keep n full backup (chains) on cleanup\n"
  printf "    [--no-cpu-limit]                  don't limit the cpu during backup\n"
#  printf "    [--notify|n]                      send notifications\n"
  printf "    [--encryption-key keyid]          use the given gpg encryption key for backup encryption\n"
  printf "    [--exclude-filelist filelist]     use the given exclude-filelist during backup\n"
  printf "    [--ignore-within-hours n]         ignore calls to backup within n hours\n"
  printf "    [--backup-folder folder]          use the given backup folder during backup\n"
  printf "    [--temp-folder folder]            (used for discarding old backups)\n"
  printf "    [--verbose|-v]                    turn on verbose output\n"
  printf "    [--version|-V]                    print version of this program\n"
  printf "    [--yes|-y]                        turn dry run off, actually execute the scripts function!\n"
}

check_exclude_filelist() {
  if [ ! -f "$EXCLUDE_FILELIST" ]; then
    printf "Excluding file list \"$EXCLUDE_FILELIST\" is not readable. Stopping execution.\n"
    [ "$DRYRUN" != "true" ] && exit 1
    printf "Continue because dryrun is $DRYRUN.\n"
  fi
}

check_source_folder() {
  if [ ! -r "$SOURCE_FOLDER" ]; then
    printf "Folder \"$SOURCE_FOLDER\" is absent or not readable. Stopping execution.\n"
    [ "$DRYRUN" != "true" ] && exit 1
    printf "Continue because dryrun is $DRYRUN.\n"
  fi
}

check_backup_folder() {
  if [ ! -w "$BACKUP_FOLDER" ]; then
    printf "Folder \"$BACKUP_FOLDER\" is absent or not writable. Stopping execution.\n"
    [ "$DRYRUN" != "true" ] && exit 1
    printf "Continue because dryrun is $DRYRUN.\n"
  fi
}

check_encryption_key() {
  if ! gpg -K $ENCRYPTION_KEY >/dev/null 2>&1; then
    printf "Encryption key \"$ENCRYPTION_KEY\" is unknown. Stopping execution.\n"
    [ "$DRYRUN" != "true" ] && exit 1
    printf "Continue because dryrun is $DRYRUN.\n"
  fi
}

check_validity_of_backup_parameters() {
  check_key_unlocked
  check_exclude_filelist
  check_backup_folder
  check_source_folder
  check_encryption_key
}

execute_command() {
  [ "$VERBOSE" == "true" ] && printf "analyse_command\n"
  case "$1" in
    NONE)
      usage
      exit 1
      ;;
    print-config)
      printf "Configuration:\n"
      conf_file=$(realpath $(find_configuration_file))
      printf "configuration file   : ${conf_file}\n"
      printf "profile              : ${PROFILE}\n"
      hours=$(hours_from_last_run)
      printf "hours since last run : ${hours}\n"
      printf "version              : $VERSION\n"
      printf "verbose              : $VERBOSE\n"
      printf "dryrun               : $DRYRUN\n"
      printf "fakerun              : $FAKERUN\n"
      printf "encryptionkey        : $ENCRYPTION_KEY\n"
      printf "exclude_filelist     : $EXCLUDE_FILELIST\n"
      printf "source_folder        : $SOURCE_FOLDER\n"
      printf "backup_folder        : $BACKUP_FOLDER\n"
      printf "temp_folder          : $TEMP_FOLDER\n"
      printf "nocpulimit           : $NOCPULIMIT\n"
      printf "force                : $FORCE\n"
      printf "keep_n               : $KEEP_N\n"
      printf "cpulimit             : $CPULIMIT\n"
      printf "ionice               : $IONICE\n"
      printf "nice                 : $NICE\n"
      printf "duplicity            : $DUPLICITY\n"
      printf "ignore_within_hours  : $IGNORE_WITHIN_HOURS\n"
      printf "batid                : $BATID\n"
      printf "discharging          : $DISCHARGING\n"
      printf "min_discharging_level: $MIN_DISCHARGING_LEVEL\n"
      printf "backup_mode          : $BACKUP_MODE\n"
      printf "months_to_full       : $MONTHS_TO_FULL\n"
      check_validity_of_backup_parameters
      exit 0
      ;;
    collection-status)
      if [ -w "$BACKUP_FOLDER" ]; then
        eval "duplicity collection-status file://${BACKUP_FOLDER}"
        exit 0
      else
        echo "Backupfolder not mounted (\"$BACKUP_FOLDER\")"
        exit 1
      fi
      ;;
    key-status)
      [ "$VERBOSE" == "true" ] && echo "  determine key status."
      if gpg -K $ENCRYPTION_KEY >/dev/null 2>&1; then
        [ "$VERBOSE" == "true" ] && echo "key is unlocked \"${ENCRYPTION_KEY}\" ."
        exit 0
      else
        [ "$VERBOSE" == "true" ] && echo "key is LOCKED \"${ENCRYPTION_KEY}\" ."
        exit 1
      fi
      ;;
    backup)
      [ "$DRYRUN" != "false" ] && check_validity_of_backup_parameters
      [ "$DRYRUN" != "false" ] && check_battery_level
      if (( $(hours_from_last_run) < $IGNORE_WITHIN_HOURS )); then
        [ "$VERBOSE" == "true" ] && printf "ignore run since last backup is ${IGNORE_WITHIN_HOURS}h or less old.\n"
      fi
      if (( $(hours_from_last_run) >= $IGNORE_WITHIN_HOURS )) || [ $FORCE = true ]; then
        [ "$VERBOSE" == "true" ] && printf "last backup is more than ${IGNORE_WITHIN_HOURS}h old (or is forced), continue processing.\n"
        pre_backup_hook
        run_backup
        post_backup_hook
        store_last_succesful_run # reads from actually stored backup from BACKUP_DIR
      fi
      ;;
    # cleanup)
    #   check_backup_folder
    #   if [ "$DRYRUN" != "false" ]; then
    #     echo "dryrun: keep ${KEEP_N} full backups, checking for other backups that could be deleted."
    #     duplicity remove-all-but-n-full ${KEEP_N} file://${BACKUP_FOLDER}
    #   else
    #     echo "keep ${KEEP_N} full backups, delete others (if present)."
    #     duplicity remove-all-but-n-full ${KEEP_N} file://${BACKUP_FOLDER} --force
    #   fi
    #   ;;
    *)
      echo "Unknown command: \"$1\""
      usage
      exit 1
      ;;
  esac
}

hours_from_last_run() {
  FILE_WITH_LAST_RUN=$(realpath $(find_configuration_folder))/${PROFILE}.last_run
  if [ -f $FILE_WITH_LAST_RUN ]; then
    LASTDATE=$(cat $FILE_WITH_LAST_RUN)
    LASTDATE_FMT=$(date --date="$LASTDATE" +%Y-%m-%dT%H:%M)
    NOW=$(date +%Y-%m-%dT%H:%M)
    HOUR_DIFF=$(datediff "$LASTDATE_FMT" "$NOW" -f "%H")
  else
    HOUR_DIFF=999
  fi
  echo "${HOUR_DIFF}"
}

proc_option() {
  [ "$VERBOSE" == "true" ] && printf "proc_option\n"
  KEY=${1:-}
  VALUE=${2:-}
  case "$KEY" in
    backup-mode)
      BACKUP_MODE="$VALUE"
      [ "$VERBOSE" == "true" ] && echo "  use backup mode $BACKUP_MODE."
      ;;
    months-to-full)
      MONTHS_TO_FULL=$VALUE
      [ "$VERBOSE" == "true" ] && echo "  months to full $MONTHS_TO_FULL."
      ;;
    profile)
      PROFILE="$VALUE"
      [ "$VERBOSE" == "true" ] && echo "  use profile $PROFILE."
      ;;
    ignore-within-hours)
      IGNORE_WITHIN_HOURS="$VALUE"
      [ "$VERBOSE" == "true" ] && echo "  ignoring calls to backup within ${IGNORE_WITHIN_HOURS}h of last backup."
      ;;
    keep)
      KEEP_N="$VALUE"
      [ "$VERBOSE" == "true" ] && echo "  keeping ${KEEP_N} full backups on cleanup."
      ;;
    fake-run)
      FAKERUN=true
      [ "$VERBOSE" == "true" ] && echo "  executing a fake run."
      ;;
    folder)
      SOURCE_FOLDER=$(realpath $(echo $VALUE | sed "s|^\~\(.*\)|$HOME\1|g"))
      [ "$VERBOSE" == "true" ] && echo "  use ${SOURCE_FOLDER} as source folder."
      ;;
    force)
      FORCE=true
      [ "$VERBOSE" == "true" ] && echo "  force to continue."
      ;;
    no-cpu-limit)
      NOCPULIMIT=true
      [ "$VERBOSE" == "true" ] && echo "  cpu limit is turned off."
      ;;
#    notify|n)
#      NOTIFICATIONS=true
#      [ "$VERBOSE" == "true" ] && echo "  notification turned on."
#      ;;
    encryption-key)
      ENCRYPTION_KEY="$VALUE"
      [ "$VERBOSE" == "true" ] && echo "using encryption key: \"$ENCRYPTION_KEY\""
      ;;
    exclude-filelist)
      EXCLUDE_FILELIST=$(realpath $(echo $VALUE | sed "s|^\~\(.*\)|$HOME\1|g"))
      [ "$VERBOSE" == "true" ] && echo "using exclude file list: \"$EXCLUDE_FILELIST\""
      ;;
    backup-folder)
      BACKUP_FOLDER="$VALUE"
      [ "$VERBOSE" == "true" ] && echo "using backupfolder \"$VALUE\""
      ;;
    temp-folder)
      TEMP_FOLDER="$VALUE"
      [ "$VERBOSE" == "true" ] && echo "using temp-folder \"$VALUE\""
      ;;
    version|V)
      echo "Version: $VERSION"
      ;;
    yes|y)
      [ "$VERBOSE" == "true" ] && echo "setting dryrun to false => the execution of the program will actually be carried out!"
      DRYRUN=false
      if [ "$IN_PREPROCESSING" != true ]; then
        check_validity_of_backup_parameters
      fi
      ;;
    verbose|v)
      VERBOSE=true
      [ "$VERBOSE" == "true" ] && echo "verbosity is turned on"
      ;;
    *)
      printf "Sorry, I didn't understand option with key: \"$KEY\" and value: \"$VALUE\"."
      usage
      exit 1
      ;;
  esac
  return 0
}

read_configuration() {
  [ "$VERBOSE" == "true" ] && printf "read_configuration"
  CONF=$(realpath $(find_configuration_file))
  if [ -f "$CONF" ]; then
    [ "$VERBOSE" == "true" ] && echo "  using configuration file \"$CONF\""
    IFS=""
    CURRENT_PROFILE="--UNKNOWN--"
    while read LINE
      do
        MAYBE_PROFILE=$(echo "$LINE" | sed "s/\(\\[\(.*\)\\]\|.*\)/\2/g")
        if [ "$MAYBE_PROFILE" != "" ]; then
          CURRENT_PROFILE=$MAYBE_PROFILE
          [ "$VERBOSE" == "true" ] && printf "processing config entries for profile $CURRENT_PROFILE\n"
        else
          if [ "$PROFILE" == "$CURRENT_PROFILE" ]; then
            proc_configuration_line $LINE
          elif [ "${LINE:0:1}" != "#" ]; then
            [ "$VERBOSE" == "true" ] && echo "skipping config line (not matching profile) '${LINE}'"
          fi
        fi
    done < $CONF
  else
    [ "$VERBOSE" == "true" ] && echo "  no configuration found."
  fi
}

read_cli_arguments() {
  [ "$VERBOSE" == "true" ] && printf "read_cli_arguments\n"
  while [[ $# > 0 ]] ; do
    KEY=$(echo $1 | sed 's/^--\?\(.*\)/\1/g')
    if [[ "${2:-}" =~ ^-.* ]] || [[ "${2-}" == "" ]]; then
      [ "$VERBOSE" == "true" ] && echo "  cli flag identified: \"$KEY\""
      proc_configuration_line "$KEY:"
    else
      [ "$VERBOSE" == "true" ] && echo "  cli key: \"$KEY\" value: \"$2\""
      proc_configuration_line "$KEY: $2"
      shift
    fi
    shift
  done
}

get_last_full() {
  LAST_FULL=$(duplicity collection-status file://${BACKUP_FOLDER} 2>&1 | head -n 1 | sed "s/Last full backup date: \(.*\)/\1/g" )
  echo $(date --date="$LAST_FULL" +%Y-%m-%d)
}

store_last_succesful_run() {
  if [ -w "$BACKUP_FOLDER" ]; then
     lastRunString=$(duplicity collection-status file://${BACKUP_FOLDER} | grep "Chain end time" | tail -n 1 | sed "s/Chain end time: \(.*\)/\1/g" )
     [ "$VERBOSE" == "true" ] && printf "recording last successfull run (${lastRunString})\n"
     FILE_FOR_LAST_RUN=$(realpath $(find_configuration_folder))/${PROFILE}.last_run
     echo "${lastRunString}" > $FILE_FOR_LAST_RUN
  fi
}

months_to_last_full_backup() {
  LAST=$(get_last_full)
  NOW=$(date +%Y-%m-%d)
  MONTH_DIFF=$(datediff "$LAST" "$NOW"  -f "%m")
  echo $MONTH_DIFF
}

run_backup() {
  printf "================================================================================\n\n"
  printf "PROFILE: ${PROFILE}\n"
  date
  printf "\n================================================================================\n"
  [ "$VERBOSE" == "true" ] && printf "run_backup\n"
  if [ "$NOCPULIMIT" == "true" ]; then
    CPULIMIT=100
  elif [ "$DRYRUN" == true ]; then
    [ "$VERBOSE" == "true" ] && printf "no date check for cpulimit, since dry running.\n"
    CPULIMIT=10
  else
    if (( $(months_to_last_full_backup) >= $MONTHS_TO_FULL )) || [ "$BACKUP_MODE" == "full" ]; then
      [ "$VERBOSE" == "true" ] && printf "full backup > ${MONTHS_TO_FULL}M old or full backup requested.\n"
      CPULIMIT=100
    else
      [ "$VERBOSE" == "true" ] && printf "full backup < ${MONTHS_TO_FULL}M old (incremental possible).\n"
      CPULIMIT=10
    fi
  fi
  [ "$VERBOSE" == "true" ] && printf "running backup with cpulimit: $CPULIMIT\n"

  CMD="  --full-if-older-than ${MONTHS_TO_FULL}M \
         --encrypt-key ${ENCRYPTION_KEY} \
         --verbosity 4 \
         --allow-source-mismatch \
         --use-agent \
         --gpg-options \"--no-tty --pinentry-mode error\" \
         --exclude-filelist ${EXCLUDE_FILELIST} \
         ${SOURCE_FOLDER} file://${BACKUP_FOLDER}"
  [ "$VERBOSE" == "true" ] && printf "running incremental backup with parameter:\n$CMD\n"
  if [ "$DRYRUN" == "false" ]; then
#    if [ "$NOTIFICATIONS" == "true" ]; then
#      notify-send "Starting backup ${PROFILE} ..."
#    fi
    eval "nice -n 19 ionice -c 3 cpulimit -l $CPULIMIT -f duplicity -- incr $CMD"
#    if [ "$NOTIFICATIONS" == "true" ]; then
#      notify-send "Finished backup ${PROFILE}, exit code $? !"
#    fi
  elif [ "$FAKERUN" == "true" ]; then
    printf "Running fake run.\n"
    eval "nice -n 19 ionice -c 3 cpulimit -l $CPULIMIT -f duplicity -- incr --dry-run $CMD"
    printf "\nWARNING: actual backup was skipped.\nIf you want to actually execute the backup, use the option '--yes'\n"
  else
    printf "WARNING: actual backup step is skipped.\nIf you want to actually execute the backup, use the option '--yes'\n"
  fi
}

check_key_unlocked() {
  [ "$VERBOSE" == "true" ] && printf "check_key_unlocked\n"
  LOCKED=$(echo "ok" | gpg --sign --batch --no-tty --pinentry-mode error --local-user ${ENCRYPTION_KEY} -o /dev/null 2>&1 || true)

  if [[ "$LOCKED" != "" ]]; then
    printf "Encryption key ${ENCRYPTION_KEY} is currently locked.\nPlease unlock the key before calling (to enable manifest processing).\n"
    # [ "$NOTIFICATIONS" == "true" ] && notify-send "Backup key is locked"
  fi

  return 0
}

check_installation() {
  [ "$VERBOSE" == "true" ] && printf "check_installation\n"
  CPULIMIT=$(command -v cpulimit || true)
  IONICE=$(command -v ionice || true)
  NICE=$(command -v nice || true)
  DUPLICITY=$(command -v duplicity || true)
  DATEDIFF=$(command -v datediff || true)
  GPG=$(command -v gpg || true)
  # NOTIFIER=$(command -v notify-send || true)

  [ "$CPULIMIT" != "" ] && [ "$VERBOSE" == "true" ] && cpulimit --help | head -n 1 || true
  [ "$IONICE" != "" ] && [ "$VERBOSE" == "true" ] && ionice --version
  [ "$NICE" != "" ] && [ "$VERBOSE" == "true" ] && nice --version | head -n 1
  [ "$DUPLICITY" != "" ] && [ "$VERBOSE" == "true" ] && duplicity --version | head -n 1
  [ "$DATEDIFF" != "" ] && [ "$VERBOSE" == "true" ] && datediff --version | head -n 1
  [ "$GPG" != "" ] && [ "$VERBOSE" == "true" ] && gpg --version | head -n 1
  # [ "$NOTIFIER" != "" ] && [ "$VERBOSE" == "true" ] && notify-send --version | head -n 1

  [ "$CPULIMIT$IONICE$NICE$DUPLICITY$DATEDIFF$GPG" == "" ] && { printf "Please ensure cpulimit, ionice, nice, duplicity, datediff, gpg is installed\n"; exit 1; }

  return 0
}

pre_backup_hook() {
  [ "$VERBOSE" == "true" ] && printf "processing pre backup hooks\n"
  conf_folder=$(find_configuration_folder)
  find $conf_folder -name "pre.${PROFILE}.*.hook" | while read HOOK; do
    if [ "$DRYRUN" != "false" ]; then
      echo "pre hook: \"$HOOK\" would be executed (if dryrun would be false)."
    else
      $HOOK || true
    fi
  done
}

post_backup_hook() {
  [ "$VERBOSE" == "true" ] && printf "processing post backup hooks\n"
  conf_folder=$(find_configuration_folder)
  find $conf_folder -name "post.${PROFILE}.*.hook" | while read HOOK; do
    if [ "$DRYRUN" != "false" ]; then
      echo "post hook: \"$HOOK\" would be executed (if dryrun would be false)."
    else
      $HOOK || true
    fi
  done
}

export PATH=$PATH:/home/pe/bin/bin:/home/pe/bin:/home/pe/.nix-profile/bin:/etc/profiles/per-user/pe/bin
pgrep duplicity >/dev/null 2>&1 && { printf "Duplicity running. Please wait until finished.\n"; exit 1; }
COMMAND=${1:-"NONE"}
if [ $# -ge 1 ]; then
  shift
fi
IN_PREPROCESSING=true
CONFIG_SOURCE="command line"
read_cli_arguments "$@" # make sure to read profile first
CONFIG_SOURCE="configuration"
read_configuration
IN_PREPROCESSING=false
CONFIG_SOURCE="command line"
read_cli_arguments "$@" # overwriting anything that is read from config file but specified in cli
check_installation
execute_command $COMMAND
