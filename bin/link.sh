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

LNFLAGS=''

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

# Excuse the faux type inferences.

# $flags -> $path -> $path -> [char] -> null
link_file() {
    if [ "$4" == "dot" ]; then
        ln $1 "$2" "$3"/."`basename $2`"
    else
        ln $1  "$2" "$3"/"`basename $2`"
    fi
}

# $flags -> $path -> $path -> [char] -> null
link_files() {
    for file in $2/*; do
        if [ "$4" == "dot" ]; then
            ln $1 "$file" "$3"/."`basename $file`"
        else
            ln $1 "$file" "$3"/"`basename $file`"
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

# check for ~/bin
if [ ! -d $HOME/bin ]; then
    redp "Error: ~/bin not found."
    redp "Creating..."

    mkdir $HOME/bin

    greenp "Done."
fi

greenp "Done."

if [[ `uname -s` == 'Linux' ]]; then
    redp "Linux kernel detected...\nConfiguring..."

    LNFLAGS="-sTfv"

    # shared nix
    link_files $LNFLAGS $BIN $HOME/bin
    link_files $LNFLAGS $ZSH $HOME "dot"
    link_files $LNFLAGS $TMUX $HOME "dot"
    link_file $LNFLAGS $NVIM $HOME/.config
    link_file $LNFLAGS $NEOFETCH $HOME/.config

    # linux
    link_files $LNFLAGS $XORG $HOME "dot"
    link_files $LNFLAGS $REDSHIFT $HOME/.config

elif [[ `uname -s` == 'Darwin' ]]; then
    redp "Darwin kernel detected...\nConfiguring..."

    LNFLAGS="-shfv"

    # shared nix
    link_files $LNFLAGS $BIN $HOME/bin
    link_files $LNFLAGS $ZSH $HOME "dot"
    link_files $LNFLAGS $TMUX $HOME "dot"
    link_file $LNFLAGS $NVIM $HOME/.config
    link_file $LNFLAGS $NEOFETCH $HOME/.config

    # darwin
    link_file $LNFLAGS $ITERM $HOME

else
    redp "Could not find a supported kernel.\nAborting..."

    exit 1
fi

greenp "Done."
greenp "Installation complete."
