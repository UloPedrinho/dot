#!/bin/zsh

# v: resize full vertical. left, right.
# q: resize to quandrant. nw, ne, sw, se.
# h: resize full horizontal. top , bottom

if [ "$1" = "v" ]; then
    i3-resize-window.zsh "hh"
    i3-resize-window.zsh "fv"
elif [ "$1" = "q" ]; then
    i3-resize-window.zsh "hh"
    i3-resize-window.zsh "hv"
elif [ "$1" = "h" ]; then
    i3-resize-window.zsh "fh"
    i3-resize-window.zsh "hv"
fi

i3-move-float.zsh $2
