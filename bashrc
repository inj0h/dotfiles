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
MY_CONFIG_PATH="$HOME/dotfiles/lib"

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
# file_load :: String -> File
function file_load {
    [ -f $1 ] && . $1
}

# Load all them configs.
file_load "$MY_CONFIG_PATH"/alias.bash
file_load "$MY_CONFIG_PATH"/hardware.bash
file_load "$MY_CONFIG_PATH"/keybindings.bash
file_load "$MY_CONFIG_PATH"/lib.bash

# Prompt
export PS1="$SH_GREEN\u$SH_CLEAR \w \$(git_parse_branch) $SH_RED\$(git_parse_dirty)$SH_CLEAR\n$SH_GREEN\$$SH_CLEAR "
