#!/bin/bash
#
# filename:         gtk_keys.sh
# description:
#                   Enable gtk key theme (system keybindings). Tested on gtk-3.0
#                   on Ubuntu 16.04.
#
# ---------------------------------------------------------------------------- #

GTK_SETTINGS="$HOME/.config/gtk-3.0/settings.ini"
OS="Ubuntu"
KEYS="Emacs"

if [[ `uname -s` == 'Linux' ]]; then
    DISTRO=$(lsb_release -i | cut -f 2-)
    if [ $DISTRO == $OS ]; then
        gsettings set org.gnome.desktop.interface gtk-key-theme $KEYS
    fi
fi
