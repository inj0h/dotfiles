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

# Excuse the pathetic faux type inferences.
# $flag -> $regex -> $path -> [char] -> null
link() {
    for file in $DOTFILES/$2; do
        if [ "$4" == "dot" ]; then
            ln $1 $file $3/.`basename $file`
        else
            ln $1 $file $3/`basename $file`
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

# specific nix
redp "`uname -s` kernel detected."
redp "Linking files specific to system..."

if [[ `uname -s` == 'Darwin' ]]; then
    # darwin
    LNFLAGS="-shfv"

    link $LNFLAGS "iterm*" $HOME

elif [[ `uname -s` == 'Linux' ]]; then
    # linux
    LNFLAGS="-sTfv"

    link $LNFLAGS "X*" $HOME dot
    link $LNFLAGS "redshift*" $HOME/.config

fi

greenp "Done."

# shared nix
redp "Linking shared *nix files..."

link $LNFLAGS "bin/*" $HOME/bin
link $LNFLAGS "z*" $HOME dot
link $LNFLAGS "tmux*" $HOME dot
link $LNFLAGS "nvim" $HOME/.config
link $LNFLAGS "neofet*" $HOME/.config

greenp "Done."
greenp "All dotfiles linked. Have a nice day."
