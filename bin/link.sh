#!/bin/bash
#
# Filename:         link.sh
# Description:
#                   Link *nix configuration files.
#

set -e

if [ -f $HOME/dotfiles/bin/lib.sh ]; then
    echo "Dependencies found. Proceeding..."
    source $HOME/dotfiles/bin/lib.sh
else
    echo "Error: could not find dependent script(s)."
    exit 1
fi

#
# Variables
#
LNFLAGS=''

#
# Functions
#
# Excuse the pathetic faux types.
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

#
# Script
#
redp "Screening directory..."

# Check for cloned repo
if [ ! -d $HOME/dotfiles ]; then
    redp "Error: dotfiles not found.\nAborting..."

    exit 1
fi

# Check for ~/bin
if [ ! -d $HOME/bin ]; then
    redp "Error: ~/bin not found.\nCreating..."

    mkdir $HOME/bin

    greenp "Done."
fi

greenp "Done."

# Specific Nix
redp "`uname -s` kernel detected.\nLinking files specific to system..."

if [[ `uname -s` == 'Darwin' ]]; then
    LNFLAGS="-shfv"

    link $LNFLAGS "iterm*" $HOME

elif [[ `uname -s` == 'Linux' ]]; then
    LNFLAGS="-sTfv"

    link $LNFLAGS "X*" $HOME dot
    link $LNFLAGS "redshift*" $HOME/.config
fi

greenp "Done."

# Shared Nix
redp "Linking shared *nix files..."

link $LNFLAGS "bin/*" $HOME/bin
link $LNFLAGS "z*" $HOME dot
link $LNFLAGS "tmux*" $HOME dot
link $LNFLAGS "emacs.d" $HOME dot
link $LNFLAGS "rgignore" $HOME/.config dot

greenp "Done.\nAll dotfiles linked. Have a nice day."
