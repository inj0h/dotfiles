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

link_files() {
    for file in $1/*; do
        if [ "$3" == "dot" ]; then
            ln -sfv $file $2/.`basename $file`
        else
            ln -sfv $file $2/`basename $file`
        fi
    done
}

link_dir() {
    if [ "$3" == "dot" ]; then
        ln -sfv $1 $2/.`basename $1`
    else
        ln -sfv $1 $2/`basename $1`
    fi
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

redp "Configuring shared *nix files..."

link_files $ZSH $HOME "dot"
link_files $TMUX $HOME "dot"

redp "Configuring shared *nix directories..."

link_dir $NVIM $HOME/.config "dot"
link_dir $NEOFETCH $HOME/.config "dot"

greenp "Done."

if [[ `uname -s` == 'Linux' ]]; then
    redp "Linux kernel detected...\nConfiguring..."

elif [[ `uname -s` == 'Darwin' ]]; then
    redp "Darwin kernel detected...\nConfiguring..."

    link_dir $ITERM $HOME
else
    redp "Could not find a supported kernel.\nAborting..."

    exit 1
fi

greenp "Done."
greenp "Installation complete."