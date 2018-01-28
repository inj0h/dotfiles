#!/bin/bash
#
# filename:         plug.sh
# description:
#                   Install and update Pathogen-managed Vim plugins.
# ---------------------------------------------------------------------------- #

set -e

# variables.
# colors
CLEAR='\033[0m'
GREEN='\033[1;32m'
RED='\033[1;31m'

# path
DIR_VIM="$HOME/dotfiles/config/nvim/bundle"

# functions
greenp() {
    echo -e ${GREEN}$1${CLEAR}
    sleep 0.5
}

redp() {
    echo -e ${RED}$1${CLEAR}
    sleep 1
}

checksum() {
    cat $(pwd)/plugins.txt | wc -l
}

# script
redp "Screening directory..."

# check path
if [ $(pwd) != $DIR_VIM ]; then
    redp "Error: running script on invalid path."
    redp "Aborting..."
    exit 1
fi

# check plugin file
if [ ! -f $(pwd)/plugins.txt ]; then
    redp "Error: no plugin file found."
    redp "Aborting..."
    exit 1
fi

# check input
if [ "$1" == "" ]; then
    redp "Error: arguments required."
    redp "Aborting..."
    exit 1
fi

# install
if [ "$1" == "install" ]; then
    if [ $(ls -A1 | grep -v "^plug" | wc -l) != 0 ]; then
        redp "Error: dirty directory."
        redp "Aborting..."
        exit 1
    fi
    redp "Installing plugins..."

    cat $(pwd)/plugins.txt | xargs -L1 git clone

    redp "Validating checksum..."
    greenp "$(ls -A1 | grep -v "^plug" | wc -l)/`checksum` plugins installed."
fi

# update
if [ "$1" == "update" ]; then
    if [ $(ls -A1 | grep -v "^plug" | wc -l) == 0 ]; then
        redp "Error: no plugins installed."
        redp "Aborting..."
        exit 1
    fi

    redp "Updating plugins..."

    for PACKAGE in $(ls -A1 | grep -v "^plug"); do
        (cd $PACKAGE && git pull);
    done

    greenp "Plugins updated."
fi
