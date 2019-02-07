# Filename:    bashrc
# Description: A good enough bashrc for most *nix users.
#

#
# Preemption
#
# If not running interactively, don't do anything.
# (Stolen from Ubuntu default)
case $- in
    *i*) ;;
    *) return;;
esac

# Append to the history file rather than overwriting it.
# (Stolen from Ubuntu default)
shopt -s histappend

#
# Variables
#
# Paths
MY_CONFIG_PATH="$HOME/dotfiles/bash.d/"

# History
# (Stolen from Ubuntu default)
# HISTCONTROL :: Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=10000

#
# Functions
#
# Excuse these pathetic fake types.
#
# load_file :: String -> File
function load_file {
    if [ -f $1 ]; then
        source $1
    fi
}

# Load all them configs.
load_file "$MY_CONFIG_PATH"alias.bash
load_file "$MY_CONFIG_PATH"hardware.bash
load_file "$MY_CONFIG_PATH"keybindings.bash
load_file "$MY_CONFIG_PATH"lib.bash

# Prompt
export PS1="$SH_GREEN\u$SH_CLEAR \w \$(git_parse_branch) $SH_RED\$(git_parse_dirty)$SH_CLEAR\n$SH_GREEN\$$SH_CLEAR "
