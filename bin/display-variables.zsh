#!/bin/zsh

# Display, current window and i3-bar data
# TODO: more than one display

# * Display info
# size
DISPLAYSIZE=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/')
DISPLAYWIDTH=$(echo $DISPLAYSIZE | sed -r 's/x.*//')
DISPLAYHEIGHT=$(echo $DISPLAYSIZE | sed -r 's/.*x//')
# center
D_CENTERX=$(($DISPLAYWIDTH/2))
D_CENTERY=$(($DISPLAYHEIGHT/2))

# * Window info
WINDOW_INFO=$(xwininfo -id $(xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)"| cut -d"#" -f2))
# size
W_WIDTH=$(echo $WINDOW_INFO| grep "Width"| cut -d":" -f2)
W_HEIGHT=$(echo $WINDOW_INFO| grep "Height"| cut -d":" -f2)
# top left corner position
W_POSX=$(echo $WINDOW_INFO| grep "Absolute upper-left X"| cut -d":" -f2)
W_POSY=$(echo $WINDOW_INFO| grep "Absolute upper-left Y"| cut -d":" -f2)

# * i3-bar height ;;  FIXME: poor and bad solution..
## get i3 bar pango font size
i3_CONFIG_FILE=~/.config/i3/config
BAR_FONT_SIZE=$(awk '/bar {/,/}/' $i3_CONFIG_FILE | grep "font pango" | rev | cut -d" " -f 1 | rev)
## calculate i3-bar height
i3_BAR_HEIGHT=$(($BAR_FONT_SIZE*2))
## set display vertical origin to 0+i3-bar-height
DISPLAY_TOP_Y=$((0+$i3_BAR_HEIGHT))

