#!/bin/zsh

if [[ $1 == 1 ]] ; then
    DATA="run,window"
    SHOW="run"
else
    DATA="window"
    SHOW="window"
fi

rofi -show-icons -modi $DATA -show $SHOW -width 40 -location 0 -lines 15 -bw 0 -yoffset 0 -color-normal "#050505,#3a5fcd,#050505,#383838,#eee8d5" -color-window "#050505,#3a5fcd,#000000"
