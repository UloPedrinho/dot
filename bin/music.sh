#!/bin/sh

MUSIC_DIR=$HOME/user/media/music/songs/

mpv --title='music.sh || ${filename}' --force-window=yes --shuffle $MUSIC_DIR  --input-ipc-server=/tmp/mpvsocket &
