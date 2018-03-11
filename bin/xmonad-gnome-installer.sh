#/!bin/bash

if [ -f /etc/os-release ]; then
    . /etc/os-release
    OS=$NAME
    VER=$VERSION_ID
    if [ $OS = "Ubuntu" ]; then
        sudo add-apt-repository ppa:gekkio/xmonad
        sudo apt-get update
        sudo apt-get install gnome-session-xmonad
    fi
else
    exit 1
fi
