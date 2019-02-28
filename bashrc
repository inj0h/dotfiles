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
my_config_path="$HOME/dotfiles/lib"

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
file_load "$my_config_path"/alias.bash
file_load "$my_config_path"/hardware.bash
file_load "$my_config_path"/keybindings.bash
file_load "$my_config_path"/lib.bash

# Prompt
export PS1="$sh_green\u$sh_clear -> \$(pwd -P) \$(git_status_display) $sh_red\$(git_parse_dirty)$sh_clear\n$sh_green\$$sh_clear "

# Completion
if [[ `uname -s` == 'Linux' ]]; then
    . /etc/profile.d/bash_completion.sh
fi
