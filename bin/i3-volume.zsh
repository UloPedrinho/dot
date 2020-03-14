#!/bin/zsh

ICON_VOLUME_ON=/usr/share/icons/gnome/48x48/status/audio-volume-high.png
ICON_VOLUME_OFF=/usr/share/icons/gnome/48x48/status/audio-volume-muted.png

volumeLevel() {
    echo $(amixer -D pulse sget Master|tail -1| cut -d" " -f7 | tr --delete "[:punct:]")%
}

isMuted() {
    if [ $(echo $(amixer -D pulse sget Master|tail -1| cut -d " " -f8 | tr -d \[\])) = "off" ] ; then
        true
    else
        false
    fi
}

if [[ $1 != 0 ]] ; then
    if [[ $1 == 1 ]] ; then
        amixer -D pulse sset Master 5%+ && pkill -RTMIN+1 i3blocks
    else
        amixer -D pulse sset Master 5%- && pkill -RTMIN+1 i3blocks
    fi
    notify-send --expire-time=1000 --icon $ICON_VOLUME_ON $(volumeLevel)
else
    amixer sset Master toggle && killall -USR1 i3blocks
    if $(isMuted) ; then
        notify-send --expire-time=1000 --icon $ICON_VOLUME_OFF "muted"
    else
        notify-send --expire-time=1000 --icon $ICON_VOLUME_ON $(volumeLevel)
    fi
fi
