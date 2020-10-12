#!/bin/sh

MUSIC_DIR=$HOME/user/media/music/songs/
MPV_SOCKET=/tmp/mpvsocket

mpv --title='music.sh || ${filename}' --force-window=yes --script-opts=osc-visibility=always --shuffle $MUSIC_DIR --input-ipc-server=$MPV_SOCKET

# delete socket
rm /tmp/mpvsocket
