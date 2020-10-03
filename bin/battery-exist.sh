#!/bin/sh

BATTERY=$(upower --enumerate| grep -i "battery_BAT")

if [[ $BATTERY == 0]]
then
    BATX=$(upower --enumerate| grep -i "battery_BAT" | rev | cut -d "/" -f1 )
    POWER=$(acpi -b | grep "${BATX[1]}:" | grep -E -o '[0-9]+[0-9][0-9]?%')
    echo "Battery: $POWER"
    echo "BAT: $POWER"

    [ ${BAT%?} -le 5 ] && exit 33
    [ ${BAT%?} -le 20 ] && echo "#FF8000"
fi

exit 0
