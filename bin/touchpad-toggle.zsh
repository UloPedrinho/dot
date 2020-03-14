#!/usr/bin/zsh

# install 'xautomation' package to move mouse
DISPLAYSIZE= xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/'
DISPLAYWIDTH= $(echo $DIMENSIONS | sed -r 's/x.*//')
DISPLAYHEIGHT= $(echo $DIMENSIONS | sed -r 's/.*x//')

# XCENTER=$(($DISPLAYWIDTH/2))
# YCENTER=$(($DISPLAYHEIGHT/2))

# MOUSETOCENTER= $(echo "mousemove $XCENTER $YCENTER")


TOUCHPADSTATE=$(synclient -l | grep "TouchpadOff"|  cut --delimiter="=" -f2 | sed 's/ //')

if [[ $TOUCHPADSTATE == 1 ]]
then
    synclient TouchpadOff=0
    # xte $MOUSETOCENTER
else
    synclient TouchpadOff=1
fi

toggle-mouse-pointer.zsh
