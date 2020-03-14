#!/bin/zsh

TELINK_ID=$(xinput --list | grep "Telink Wireless Receiver Mouse" | sed -n 's:.*id=\(.*\)\t.*:\1:p')

if [[ -n $TELINK_ID ]]
then
    TELINK_STATE=$(xinput --list-props $TELINK_ID | grep "Device Enabled" | cut -d ":" -f 2| sed --expression='s/[ \t]//g')
    if [[ $TELINK_STATE == 1 ]]
    then
        move-mouse-to-display-point.zsh $TELINK_ID corner
        xinput set-int-prop $TELINK_ID "Device Enabled" 8 0
    else
        xinput set-int-prop $TELINK_ID "Device Enabled" 8 1
        move-mouse-to-display-point.zsh $TELINK_ID center
    fi
fi


