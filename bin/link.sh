#!/bin/bash
#
# filename:         link.sh
# description:
#                   Link erikorojo's *nix configuration files.
#
# ------------------------------------------------------------------------------

set -e

############
# variables
############

# colors
CLEAR='\033[0m'
GREEN='\033[1;32m'
RED='\033[1;31m'

# paths
DOTFILES="$HOME/dotfiles"

BIN="$DOTFILES/bin"
CONFIG="$DOTFILES/config"

######
# nix
######

ZSH="$CONFIG/zsh"
TMUX="$CONFIG/tmux"
NVIM="$CONFIG/nvim"
NEOFETCH="$CONFIG/neofetch"

########
# linux
########

REDSHIFT="$CONFIG/redshift"
XORG="$CONFIG/xorg"
XMONAD="$CONFIG/xmonad"

#########
# darwin
#########

ITERM="$CONFIG/iterm.d"

############
# functions
############

greenp() {
    echo -e ${GREEN}$1${CLEAR}
    sleep 0.5
}

redp() {
    echo -e ${RED}$1${CLEAR}
    sleep 1
}

link_file() {
    if [ "$3" == "dot" ]; then
        ln -sTfv "$1" "$2"/."`basename $1`"
    else
        ln -sTfv "$1" "$2"/"`basename $1`"
    fi
}

link_files() {
    for file in $1/*; do
        if [ "$3" == "dot" ]; then
            ln -sTfv "$file" "$2"/."`basename $file`"
        else
            ln -sTfv "$file" "$2"/"`basename $file`"
        fi
    done
}

link_file_darwin() {
    if [ "$3" == "dot" ]; then
        ln -shfv "$1" "$2"/."`basename $1`"
    else
        ln -shfv "$1" "$2"/"`basename $1`"
    fi
}

link_files_darwin() {
    for file in $1/*; do
        if [ "$3" == "dot" ]; then
            ln -shfv "$file" "$2"/."`basename $file`"
        else
            ln -shfv "$file" "$2"/"`basename $file`"
        fi
    done
}

#########
# script
#########

redp "Screening directory..."

# check for cloned repo
if [ ! -d $HOME/dotfiles ]; then
    redp "Error: dotfiles not found."
    redp "Aborting..."
    exit 1
fi

greenp "Done."

if [[ `uname -s` == 'Linux' ]]; then
    redp "Linux kernel detected...\nConfiguring..."

    # shared nix
    link_files $ZSH $HOME "dot"
    link_files $TMUX $HOME "dot"
    link_file $NVIM $HOME/.config
    link_file $NEOFETCH $HOME/.config

    # linux
    link_files $XORG $HOME "dot"
    link_files $REDSHIFT $HOME/.config

elif [[ `uname -s` == 'Darwin' ]]; then
    redp "Darwin kernel detected...\nConfiguring..."

    # shared nix
    link_files_darwin $ZSH $HOME "dot"
    link_files_darwin $TMUX $HOME "dot"
    link_file_darwin $NVIM $HOME/.config
    link_file_darwin $NEOFETCH $HOME/.config

    # darwin
    link_file_darwin $ITERM $HOME

else
    redp "Could not find a supported kernel.\nAborting..."

    exit 1
fi

greenp "Done."
greenp "Installation complete."
