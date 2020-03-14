#!/bin/zsh

# timidity -iA > /dev/null 2>&1 &
fluidsynth -s -a pulseaudio -i -m alsa_seq -o midi.alsa_seq.id="fluidsynth" /usr/share/sounds/sf2/FluidR3_GM.sf2 &

# wait to until fluidsynth server has started
# 0.5 ?
sleep 0.5

# start kmetronome
kmetronome

# kill server when kmetronome ends
pkill fluidsynth
