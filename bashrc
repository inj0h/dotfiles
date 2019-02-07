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
# Bash prompt colors (Bash CLear|GReen|Red)
BCL="\[\033[0m\]"
BGR="\[\033[0;32m\]"
BRD="\[\033[0;31m\]"

# Config filenames
MY_CONFIG=(alias hardware keybindings lib)
MY_CONFIG_PATH="$HOME/bin/"

# Git
GIT_STAT_CLEAN="nothing to commit, working tree clean"
GIT_SIGIL_DIRTY="X"

# History
# (Stolen from Ubuntu default)
# HISTCONTROL :: Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=10000

#
# Functions
#
# parse_git_dirty :: String -> String
function parse_git_dirty {
    [[ $(git status 2> /dev/null | tail -n1) != "$GIT_STAT_CLEAN" &&
           $(git status 2> /dev/null | tail -n1) != "" ]] &&
        echo "$GIT_SIGIL_DIRTY"
}

# parse_git_branch :: String -> String
function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1]/"
}

# Prompt
export PS1="$BGR\u$BCL \w \$(parse_git_branch) $BRD\$(parse_git_dirty)$BCL\n$BGR\$$BCL "

# Load all the configs!
for config in "${MY_CONFIG[@]}"; do
    if [ -f "$MY_CONFIG_PATH$config" ]; then
        source $config
    fi
done
