# Filename: bashrc
# Note:     A good enough bashrc for most *nix users.
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
export PS1="$sh_green\u$sh_clear $sh_cyan\$(pwd -P)$sh_clear $sh_blue\$(git_status_display)$sh_clear $sh_red\$(git_parse_dirty)$sh_clear\n$sh_green\$(u_lambda)$sh_clear "

# Externalities
#
# Completion
[[ `uname -s` == 'Linux' ]] &&
    . /etc/profile.d/bash_completion.sh

# Fzf
[ -f ~/.fzf.bash ] && . ~/.fzf.bash
export FZF_DEFAULT_OPTS='--color bg:-1,bg+:-1,fg+:-1'

command -v rg > /dev/null &&
    export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'

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

command -v brew &&
    [ $(brew list | grep nvm) == "nvm" ] &&
    my_nvm="/usr/local/opt/nvm/nvm.sh" &&
    my_nvm_complete="/usr/local/opt/nvm/etc/bash_completion"

[ -s $my_nvm ] && . $my_nvm
[ -s $my_nvm_complete ] && . $my_nvm_complete
