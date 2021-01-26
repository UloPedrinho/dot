#!/bin/zsh

# xte command: install 'xautomation' package to move mouse

DISPLAYSIZE=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/')
DISPLAYWIDTH=$(echo $DISPLAYSIZE | sed -r 's/x.*//')
DISPLAYHEIGHT=$(echo $DISPLAYSIZE | sed -r 's/.*x//')

XCENTER=$(($DISPLAYWIDTH/2))
YCENTER=$(($DISPLAYHEIGHT/2))


if [ "$2" = "center" ]
then
    MOUSETOPOINT=$(echo "mousemove $XCENTER $YCENTER")
else if [ "$2" = "corner" ]
     then
        MOUSETOPOINT=$(echo "mousemove $DISPLAYWIDTH $DISPLAYHEIGHT")
     fi
fi

xte -i $1 $MOUSETOPOINT
