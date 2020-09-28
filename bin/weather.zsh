#!/bin/zsh

LOG_FILE=~/org/various/weather.log
DATE=$(date "+%d%m%Y%H%M")
WEATHER=$(curl -s "wttr.in/lugo?format=%c+%t+%w")

if [ "${WEATHER: -4}" = "km/h" ]
then
    # logging to file
    echo $DATE:$WEATHER >> $LOG_FILE
    # weather output
    echo $WEATHER
else
    echo "NO WEATHER AVAILABLE!!"
fi
