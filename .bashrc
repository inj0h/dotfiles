# Filename: bashrc
# Note:     A good enough bashrc for most *nix users.

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

# System Variables

# History
# (Stolen from Ubuntu default)
# HISTCONTROL :: Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=10000

# User Variables

# Colors

# These get used in the prompt. I forget what the escaped bracket means.
sh_blue="\[\033[0;34m\]"
sh_clear="\[\033[0m\]"
sh_cyan="\[\033[0;36m\]"
sh_green="\[\033[0;32m\]"
sh_purple="\[\033[0;35m\]"
sh_red="\[\033[0;31m\]"

# These are for printing to STDOUT.
echo_clear='\033[0m'
echo_green='\033[1;32m'
echo_red='\033[1;31m'

# Git
git_stat_clean="nothing to commit, working tree clean"

# Paths
my_config_path="$HOME/dotfiles/lib"

# Unicode
utf8_arrow_ltr="\u21fe"
utf8_lambda="\u03bb"
utf8_x="\u2717"

#
# Functions
#

# Print UTF-8 symbols

u_x() {
    printf "$utf8_x"
}

u_lambda() {
    printf "$utf8_lambda"
}

u_arrow_ltr() {
    printf "$utf8_arrow_ltr"
}

# PS1 Git status, etc.

# TODO: Write documentation from "type signature"
# git_parse_branch :: Exit -> String -> String
git_parse_branch() {
    git_status_check &&
        git branch --no-color 2> /dev/null |
            sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/"
}

# TODO: Write documentation from "type signature"
# git_parse_dirty :: Exit -> Exit -> Exit -> String
git_parse_dirty() {
    git_status_check &&
        git status 2> /dev/null |
                       tail -n1 | grep "$git_stat_clean" > /dev/null 2>&1
    [ $? = 1 ] && u_x
}

# TODO: Write documentation from "type signature"
# git_parse_repo :: Exit -> String -> String
git_parse_repo() {
    git_status_check &&
        basename "$(git rev-parse --show-toplevel)"
}

# TODO: Write documentation from "type signature"
# git_status_check :: String -> Exit
git_status_check() {
    git rev-parse --is-inside-work-tree > /dev/null 2>&1
}

# TODO: Write documentation from "type signature"
# git_status_display :: Exit -> String -> String -> String
git_status_display() {
    git_status_check &&
        echo "[$(git_parse_repo) $(u_arrow_ltr) $(git_parse_branch)]"
}

#
# Prompt
#

export PS1="$sh_green\u$sh_clear $sh_cyan\$(pwd -P)$sh_clear $sh_blue\$(git_status_display)$sh_clear $sh_red\$(git_parse_dirty)$sh_clear\n$sh_green\$(u_lambda)$sh_clear "

#
# Externalities
#

# Completion
[[ `uname -s` == 'Darwin' ]] &&
    . /usr/local/etc/bash_completion

[[ `uname -s` == 'Linux' ]] &&
    . /etc/profile.d/bash_completion.sh

# Haskell
#
# Stack
[[ `uname -s` == 'Linux' ]] &&
    export PATH=$PATH:/home/echung/.local/bin

# Homebrew
[[ `uname -s` == 'Darwin' ]] &&
    export PATH="/usr/local/sbin:$PATH"

# Node
#
# NVM
export NVM_DIR="$HOME/.nvm"

command -v brew > /dev/null &&
    [ $(brew list | grep nvm) == "nvm" ] &&
    my_nvm="/usr/local/opt/nvm/nvm.sh" &&
    my_nvm_complete="/usr/local/opt/nvm/etc/bash_completion"

[ -s $my_nvm ] && . $my_nvm
[ -s $my_nvm_complete ] && . $my_nvm_complete
