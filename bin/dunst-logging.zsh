#!/bin/zsh

# from .dunstrc:
#   script appname summary body icon urgency

LOG_FILE=~/tmp/logs/dunst.log

# if [ ! -a $LOG_FILE ] ; then
#     touch $LOG_FILE
# fi

echo $(date "+%d%m%y%H%M%S")::$1::$2::$3 >> $LOG_FILE
