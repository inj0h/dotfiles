#!/bin/bash
#
# Filename:         distingui.sh
# Description:
#                   Useful functions for "distinguish"ing IO devices on Linux.
#

if [[ `uname` == 'Linux' ]]; then
    touchpad() {
        # Toggle your touchpad
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
