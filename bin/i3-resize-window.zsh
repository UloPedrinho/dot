#!/bin/zsh

# Resize the window to:
# fv :: full vertical
# fh :: full horizontal
# hv :: half vertical
# hh :: half horizontal
SIZE=$1

# * load display and window variables
. display-variables.zsh

# * New window size
SIZE=$(case "$SIZE" in
           "fv") echo "$W_WIDTH $(($DISPLAYHEIGHT-$DISPLAY_TOP_Y))" ;;
           "hv") echo "$W_WIDTH $(($DISPLAYHEIGHT/2))" ;;
           "hh") echo "$(($DISPLAYWIDTH/2)) $W_HEIGHT" ;;
           "fh") echo "$DISPLAYWIDTH $W_HEIGHT" ;;
       esac)

# * Set window size
i3-msg "resize set $SIZE"

# echo $SIZE
