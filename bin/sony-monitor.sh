#!/bin/sh
xrandr --output HDMI-1 --mode 1920x1080 --same-as LVDS-1 --output LVDS-1 --off

pacmd set-card-profile 0 output:hdmi-stereo+input:analog-stereo
