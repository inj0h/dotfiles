# Filename:    keybindings.bash
# Description: Manage keybindings on Linux.
#

GTK_SETTINGS="$HOME/.config/gtk-3.0/settings.ini"
OS="Ubuntu"
KEYS="Emacs"

if [[ `uname -s` == 'Linux' ]]; then
    DISTRO=$(lsb_release -i | cut -f 2-)
    if [ $DISTRO == $OS ]; then
        gsettings set org.gnome.desktop.interface gtk-key-theme $KEYS
    fi
fi
