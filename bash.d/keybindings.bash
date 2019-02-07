# Filename:    keybindings.bash
# Description: Manage keybindings on Linux.
#

#
# Linux
#
# Remap capslock to control.
[[ `uname -s` == 'Linux' ]] && setxkbmap -option caps:ctrl_modifier
