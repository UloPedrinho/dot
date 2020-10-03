#!/bin/zsh

BATTERY=$(upower --enumerate| grep -i "battery_BAT")

if [[ $? == 0 ]]
then
    BATX=$(upower --enumerate| grep -i "battery_BAT" | rev | cut -d "/" -f1 )
    POWER=$(acpi -b | grep "${BATX[1]}:" | grep -E -o '[0-9]+[0-9][0-9]?%')
    echo "Battery: $POWER"
    echo "BAT: $POWER"

    [[ ${POWER%?} -le 5 ]] && exit 33
    [[ ${POWER%?} -le 20 ]] && echo "#FF8000"
    [[ ${POWER%?} -ge 60 ]] && echo "#00ff00"
fi

exit 0
