
#!/bin/sh

MUSIC_DIR=$HOME/user/media/music/songs/

mpv --title='music.sh || ${filename}' --force-window=yes --script-opts=osc-visibility=always --shuffle --pause $MUSIC_DIR --input-ipc-server=/tmp/mpvsocket &
