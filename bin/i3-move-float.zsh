#!/bin/zsh

# Move the window to display position:
# nw   n  ne
#    \ | /
#  w - c - e
#    / | \
# sw   s  se
POSITION=$1

# * load display, current window and i3-bar variables
. display-variables.zsh

# * Position to move the current window.
POSITION=$(case "$POSITION" in
               "c")
               # TODO
               ;;
               "n")  echo "$W_POSX $DISPLAY_TOP_Y" ;;
               "nw") echo "0 $DISPLAY_TOP_Y" ;;
               "ne") echo "$D_CENTERX $DISPLAY_TOP_Y" ;;
               "w")  echo "0 $W_POSY" ;;
               "e")  echo "$D_CENTERX $W_POSY" ;;
               "s")  echo "$W_POSX $D_CENTERY" ;;
               "sw") echo "0 $D_CENTERY" ;;
               "se") echo "$D_CENTERX $D_CENTERY" ;;
           esac)

i3-msg "move position $POSITION"
