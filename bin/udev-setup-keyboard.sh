#!/bin/sh
# Keyboard setup to be called from udev rule

set -u    # exit if undefined var used

# exit if X isn't up get
[ -e $XAUTHORITY ] || exit

# - caps is hyper
# - make compose key be right control
/usr/bin/setxkbmap -option '' -option 'caps:hyper' -option 'compose:rctrl'

# Put hyper key on mod4 as the Unix gods intended
xkb=${HOME}/.config/xkb
if [ -r "${xkb}" ]; then
	 /usr/bin/setxkbmap -print | /usr/bin/sed -e '/xkb_symbols/s/"[[:space:]]/+local&/' | /usr/bin/xkbcomp -I${xkb} - ${DISPLAY} &> /dev/null
fi
