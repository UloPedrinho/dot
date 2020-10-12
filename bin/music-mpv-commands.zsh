#!/bin/zsh

MPV_SOCKET=/tmp/mpvsocket


# launch 'music.sh' if not active
if [[ ! -S $MPV_SOCKET ]] ; then
    music.sh &
fi

COMMAND=$(case "$1" in
              "play")
                  msg=$(echo '{ "command": ["get_property", "pause"] }' | socat - /tmp/mpvsocket)
                  msg=("${(@s/:/)msg}")
                  msg=("${(@s/,/)msg}")

                  if [ $msg[2] = "true" ] ; then
                      echo '{ "command": ["set_property", "pause", false] }'
                  else
                      echo '{ "command": ["set_property", "pause", true] }'
                  fi
                  ;;
              "next")
                  echo '{ "command": ["playlist-next"] }'
                  ;;
              "prev")
                  echo '{ "command": ["playlist-prev"] }'
                  ;;
          esac)

echo $COMMAND | socat - $MPV_SOCKET
