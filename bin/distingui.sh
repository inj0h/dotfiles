#!/bin/bash
#
# filename:         distingui.sh
# description:
#                   Useful functions for "distinguish"ing I/O devices on Linux.
#
# ------------------------------------------------------------------------------

if [[ `uname` == 'Linux' ]]; then
    touchpad() {
        # toggle your touchpad
        TOUCHPAD=`xinput list | grep TouchPad | cut -f 2 | cut -c 4-`

        if [ $1 == "on" ]; then
            xinput --enable $TOUCHPAD
        elif [ $1 == "off" ]; then
            xinput --disable $TOUCHPAD
        else
            echo "Error: please input either \`on\` or \`off\`."
            return
        fi
    }

fi
